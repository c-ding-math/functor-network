{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Entry where

import Yesod.Form.Bootstrap3
import Handler.Parser(parse,markItUpWidget)
import Parse.Parser(mdToHtml,texToHtml,scaleHeader,EditorData(..))
import Import

getEntryR :: Path ->  EntryId -> Handler Html
getEntryR _ entryId = do    
    maybeUserId<-maybeAuthId
    maybeUser<-maybeAuth
    (entry,mEntryAuthor,comments,mCommentAuthors)<-runDB $ do
        entry<-get404 entryId
        if entryStatus entry==Publish || isAdministrator maybeUserId entry
          then do
            mEntryAuthor<-getAuthor $ entryUserId entry
            comments<-selectList [EntryParentId==.Just entryId,EntryType==.Comment][Asc EntryInserted]
            mCommentAuthors<-mapM (getAuthor . entryUserId . entityVal) comments
            return $ (entry,mEntryAuthor,comments,mCommentAuthors)        
          else permissionDeniedI MsgPermissionDenied
    formatParam <- lookupGetParam "format"
    let format = case formatParam of
            Just "tex" -> Format "tex"
            _ -> Format "md"
    (commentWidget, commentEnctype) <-  generateFormPost $ newCommentForm $ Just format

    defaultLayout $ do
        setTitleI MsgEntry
        [whamlet|
<div .entry :entryStatus entry == Draft:.draft>
  <h1>#{preEscapedToMarkup(scaleHeader 1 (entryOutputTitle entry))}
  <div .entry-meta>
      <span .by>
          $maybe author<-mEntryAuthor    
              
              <a href=@{UserAboutR (entryUserId entry)}>#{userName author}
          $nothing 
              _{MsgUnknownUser}
      <span .at>#{formatDateStr (entryInserted entry)}
  <div .entry-content>
      <article>#{preEscapedToMarkup(entryOutputBody entry)}
  <ul .entry-menu>
        <li .comment><a href=#comments>comment</a>
        <!--<li .print><a href=#>print</a>-->
        $if isAdministrator maybeUserId entry
            <li .edit><a href=@{EditEntryR entryId}>edit</a>

<!--<div .entry-comment-separator>-->
<section #comments .comments>    
    $if null comments
        <p style="display:none">_{MsgNoComment}
    $else
        <h3>_{MsgComments}
        $forall  (Entity commentId comment,mCommentAuthor)<-zip comments mCommentAuthors
            <div .comment id=comment-#{toPathPiece commentId}> 
              <div .comment-meta>
                  <span .by>
                      $maybe author<-mCommentAuthor    
                          
                          <a href=@{UserAboutR (entryUserId comment)}>#{userName author}
                      $nothing 
                          _{MsgUnknownUser}
                  <span .at>#{formatDateStr (entryInserted comment)}
              <div .comment-content>
                  <article>#{preEscapedToMarkup (entryOutputBody comment)}  
              <ul .comment-menu>                
                  <!--<li .reply><a href=#comment>reply</a>-->
                  $if isAdministrator maybeUserId entry || isAdministrator maybeUserId comment
                      <li .delete>
                        <a href=@{EditCommentR commentId}>_{MsgDelete}

<section .new-comment>
        $maybe _ <- maybeUser
            <h3 #comment>_{MsgNewComment}
            <form method=post enctype=#{commentEnctype}>
                
                <!--<select>
                    $if format == Format "tex"
                        <option value=1>Markdown
                        <option value=2 selected>LaTeX
                    $else
                        <option value=1 selected>Markdown
                        <option value=2>LaTeX -->
                ^{commentWidget}
                <div>
                    <button .btn.btn-primary type=submit>_{MsgComment}
        $nothing
            <h3 #comment>_{MsgNewComment}
            <p>
                <a href=@{AuthR LoginR}>_{MsgLoginToComment}
        |]
        markItUpWidget format (Format "html")
        menuWidget

        
menuWidget::Widget
menuWidget=do
    toWidget [julius|
        $(document).ready(function()	{
            $(".entry-menu .print a").click(function(){
                window.print();
                return false;
            }); 
            $(".entry-menu .favorite a,.comment .vote a").click(function(){
                var that =$(this);
                var url=$(this).attr("href");
                $.ajax({
                    type: "POST",
                    url: url,
                    success: function(countStr){
                            if (isNaN(parseInt(countStr))){
                                alert(countStr);                              
                            } else {that.attr("data-like",countStr);}
                        },
                });
                return false;
            });       
            $(".comment-menu .delete a").click(function(event){  
                if (confirm("Are you sure that you want to delete the comment?")) {
            
                    var that =$(this);
                    var url=$(this).attr("href");
                    $.ajax({
                        type: "POST",
                        url: url,
                        success: function(){
                                that.closest(".comment").remove();
                                if ($(".comments .comment").length==0){
                                    $(".comments").html("");
                                }
                            },
                    });
                }
                return false;         
            });
        });    
    |]
    toWidget [lucius|   
.tags ul{
  list-style-type: none;
  padding-left:0;
}
.tags ul>li{
  display: inline-block;
}

.entry-meta>span.at:before,.comment-meta>span.at:before{
  content:" - ";
}
.entry-meta>span.by:after{
  content:" ";
}
.comment-meta>span.by:after{
  content:" ";
}
.entry-meta>span,.comment-meta>span{
  color:#b4bcc2;
}
.entry-meta,.comment-meta{
  margin-bottom:1em;
}
.entry-menu .favorite>a:after,.comment-menu .vote>a:after{
  content:"("attr(data-like)")";
}
.entry-menu,.comment-menu{
  list-style-type: none;
  padding-left:0;
  text-transform:lowercase;
}
.entry-menu>li,.comment-menu>li{
  display: inline-block;
  margin-right:1em;
  margin-bottom:2em;
}
.entry-menu a,.comment-menu a{
    color:#b4bcc2;
}
.entry-comment-separator{
  border-bottom:1px #b4bcc2 dashed;
  margin:2em 0 4em;
}
    |]

data CommentInput=CommentInput
    {preamble::Maybe Textarea
    ,inputFormat::Format
    ,content::Textarea
    ,citation::Maybe Textarea
    }

newCommentForm :: Maybe Format -> Form CommentInput
newCommentForm mFormat =  renderBootstrap3 BootstrapBasicForm $ CommentInput
    <$> aopt textareaField preambleSettings Nothing
    <*> areq (selectFieldList inputFormats) "Comment" mFormat
    <*> areq textareaField editorSettings Nothing
    <*> aopt textareaField citationSettings Nothing
    where   inputFormats = [("Markdown", Format "md"), ("LaTeX", Format "tex")]::[(Text, Format)] 
            editorSettings = FieldSettings
                { fsLabel = ""
                , fsTooltip = Nothing
                , fsId = Nothing
                , fsName = Just "content"
                , fsAttrs =
                    [ ("class", "editor form-control")
                    , ("placeholder", "")
                    ]
                }
            preambleSettings = FieldSettings
                { fsLabel = ""
                , fsTooltip = Nothing
                , fsId = Nothing
                , fsName = Just "preamble"
                , fsAttrs =[("class", "hidden")]
                }
            citationSettings = FieldSettings
                { fsLabel = ""
                , fsTooltip = Nothing
                , fsId = Nothing
                , fsName = Just "citation"
                , fsAttrs =[("class", "hidden")]             
                }

postEntryR :: Path ->  EntryId -> Handler Html
postEntryR _ entryId = do
    userId<-requireAuthId
    entry<-runDB $ get404 entryId
    urlRenderParams <- getUrlRenderParams
    ((res, _), _) <- runFormPost $ newCommentForm Nothing
    case res of
        FormSuccess newCommentFormData -> do
            let editorData=EditorData{
                editorPreamble=preamble newCommentFormData
                ,editorContent=Just (content newCommentFormData)
                ,editorCitation=citation newCommentFormData
            }
            currentTime <- liftIO getCurrentTime
            let parser=  case inputFormat newCommentFormData of
                        Format "tex" -> texToHtml
                        _ -> mdToHtml
            bodyHtml <- liftIO $ parse (Just userId) parser editorData
            let commentData=Entry 
                        {entryParentId=Just entryId
                        ,entryUserId=userId
                        ,entryType=Comment
                        ,entryInputFormat=inputFormat newCommentFormData
                        ,entryOutputFormat=Format "html"
                        ,entryInputTitle="Comment on "<> (entryInputTitle entry)
                        ,entryOutputTitle="Comment on "<> (entryOutputTitle entry)
                        ,entryInputPreamble=(preamble newCommentFormData)
                        ,entryInputBody=(content newCommentFormData)
                        ,entryOutputBody=bodyHtml
                        ,entryInputCitation=(citation newCommentFormData)
                        ,entryInserted=currentTime
                        ,entryUpdated=currentTime
                        --,entryStuck=Just currentTime
                        ,entryStatus=Publish
                        ,entryLocked=True
                        ,entryInputTags=[]
                        ,entryOutputTags=[]
                        }
            
            {-let commentData=Comment
                  {
                      commentEntryId=entryId
                      --,commentCommentId=commentId newCommentFormData
                      ,commentUserId = userId
                      ,commentInputFormat = Format "md"
                      ,commentOutputFormat = Format "html"
                      ,commentInputPreamble = preamble newCommentFormData
                      ,commentInputBody = content newCommentFormData
                      ,commentOutputBody = bodyHtml
                      ,commentInputCitation = citation newCommentFormData
                      ,commentInserted =currentTime
                      ,commentUpdated =currentTime
                      ,commentStuck=Nothing
                        }-}
                
            commentId <- runDB $ insert commentData
            setMessage $ [hamlet|<a href=#comment-#{toPathPiece commentId}>Your comment</a> has been published.|] urlRenderParams
            redirect $ EntryR userId entryId
        _ -> do
            setMessageI MsgSomethingWrong
            redirect $ EntryR userId entryId

formatDateStr :: UTCTime -> String
formatDateStr t = formatTime defaultTimeLocale "%e %b %Y" t

getAuthor ::  UserId -> ReaderT SqlBackend (HandlerFor App) (Maybe User)
getAuthor uid = do
    maybeAuthor<-get uid
    return maybeAuthor

