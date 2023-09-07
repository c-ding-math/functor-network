{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Entry where

import Yesod.Form.Bootstrap3
import Handler.Parser(markItUpWidget)
import Parse.Parser(scaleHeader)
import Handler.EditComment(getChildIds)
import Import

getEntryR :: UserId ->  EntryId -> Handler Html
getEntryR authorId entryId = do    
    maybeUserId<-maybeAuthId
    maybeUser<-maybeAuth
    (entry,mEntryAuthor,comments,mCommentAuthors,mParentCommentAuthors)<-runDB $ do
        entry<-get404 entryId
        if (entryUserId entry==Just authorId) && (entryStatus entry==Publish || isAdministrator maybeUserId entry) && (entryType entry==Standard)
          then do
            mEntryAuthor<- case entryUserId entry of
                Just userId -> selectFirst [UserId==.userId] []
                Nothing -> return Nothing
            commentIds<- getChildIds entryId
            comments<-selectList [EntryId <-. commentIds, EntryStatus==.Publish, EntryType==.Comment][Asc EntryInserted]     
            mCommentAuthors<-mapM (\x-> case entryUserId $ entityVal x of
                Just userId -> selectFirst [UserId==.userId] []
                Nothing -> return Nothing ) comments
            mParentCommentAuthors<-mapM (\x -> case entryParentId $ entityVal x of
                Just parentId -> do
                    mParentComment<-get parentId
                    case mParentComment of
                        Just parentComment -> do
                            mParentCommentAuthor<- case entryUserId parentComment of
                                Just userId -> selectFirst [UserId==.userId] []
                                Nothing -> return Nothing
                            return mParentCommentAuthor
                        Nothing -> return Nothing
                Nothing -> return Nothing
                ) comments
            return $ (entry,mEntryAuthor,comments,mCommentAuthors,mParentCommentAuthors)        
          else notFound

    formatParam <- lookupGetParam "format"
    let format = case formatParam of
            Just "tex" -> Format "tex"
            _ -> Format "md"
    (commentWidget, commentEnctype) <- case maybeUser of
        Nothing ->  generateFormPost $ newCommentForm Nothing
        Just (Entity _ user) -> generateFormPost $ newCommentForm $ Just $ CommentInput (userDefaultPreamble user) format (Textarea "") (userDefaultCitation user)

    defaultLayout $ do
        setTitle $ toHtml $ entryInputTitle entry
        setDescriptionIdemp $ (intercalate ";" $ entryInputTags entry) <> case mEntryAuthor of
            Just author -> "; by " <> (userName $ entityVal author)
            Nothing -> ""
        [whamlet|
<div .entry :entryStatus entry == Draft:.draft #entry-#{toPathPiece entryId}>
  <h1>#{preEscapedToMarkup(scaleHeader 1 (entryOutputTitle entry))}
  <div .entry-meta>
      <span .by>
          $maybe author<-mEntryAuthor      
              <a href=@{PageR (entityKey author) "About"}>#{userName $ entityVal author}
          $nothing 
              _{MsgUnregisteredUser}
      <span .at>#{formatDateStr (entryInserted entry)}
  <div .entry-content>
      <article>#{preEscapedToMarkup(entryOutputBody entry)}
  <ul .entry-menu>
        <li .reply>
            <a href=#comment data-action=@{EditCommentR entryId}>comment
        <!--<li .print><a href=#>print</a>-->
        $if isAdministrator maybeUserId entry
            <li .edit><a href=@{EditEntryR entryId}>edit</a>

<section #comments .comments  :entryStatus entry == Draft:.draft>    
    $if null comments
        <p style="display:none">_{MsgNoComment}
    $else
        <h3>_{MsgComments}
        $forall (Entity commentId comment,mCommentAuthor,mParentCommentAuthor)<-zip3 comments mCommentAuthors mParentCommentAuthors
            <div .comment id=entry-#{toPathPiece commentId}> 
              <div .entry-meta>
                  <span .by>
                      $maybe author<-mCommentAuthor                           
                          <a href=@{PageR (entityKey author) "About"}>#{userName $ entityVal author}
                      $nothing 
                          _{MsgUnregisteredUser}
                  <span .at>#{formatDateStr (entryInserted comment)}
                  $maybe parentCommentId<-entryParentId comment
                    $if parentCommentId /= entryId
                        <span .to>
                            <a href=#entry-#{toPathPiece parentCommentId}>
                                $maybe parentAuthor<-mParentCommentAuthor
                                    #{userName $ entityVal parentAuthor}
                                $nothing
                                    _{MsgUnregisteredUser}

              <div .entry-content>
                  <article>#{preEscapedToMarkup (entryOutputBody comment)}  
              <ul .entry-menu>                
                  <li .reply>
                    <a href=#comment data-action=@{EditCommentR commentId}>reply
                  $if isAdministrator maybeUserId entry || isAdministrator maybeUserId comment
                      <li .delete>
                        <a href=@{EditCommentR commentId}>_{MsgDelete}

<section .new-comment>
        $maybe _ <- maybeUser
            <h3 #comment>_{MsgNewComment}
            <form method=post enctype=#{commentEnctype} action=@{EditCommentR entryId}>
                
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
            /*$(".entry-menu .print a").click(function(){
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
            }); */
            $(".entry .entry-menu .reply a").click(function(){
                $("#comment").html("Add a comment");
                var handlerUrl=$(this).attr("data-action");
                $(".new-comment form").attr("action", handlerUrl);
            });
            $(".comment .entry-menu .reply a").click(function(){
                var handlerUrl=$(this).attr("data-action");
                $(".new-comment form").attr("action", handlerUrl);
                var parentId=$(this).closest(".comment").attr("id").replace("entry-","");
                var parentCommentAuthor=$(this).closest(".comment").find(".entry-meta .by a").html();
                $("#comment").html("Reply to <a href=#entry-"+parentId+">"+parentCommentAuthor+"</a>");
            });
            $(".entry-menu .delete a").click(function(event){  
                if (confirm("Are you sure that you want to delete the comment?")) {
            
                    var that =$(this);
                    var url=$(this).attr("href");
                    $.ajax({
                        type: "DELETE",
                        url: url,
                        error: function(){//maybe parent entry was deleted
                            location.reload();
                        },
                        success: function(){
                            location.reload();
                                /*that.closest(".comment").remove();
                                if ($(".comments .comment").length==0){
                                    $(".comments").html("");
                                }*/
                            },
                    });
                }
                return false;         
            });
        });    
    |]

data CommentInput=CommentInput
    {preamble::Maybe Textarea
    ,inputFormat::Format
    ,content::Textarea
    ,citation::Maybe Textarea
    }

newCommentForm :: Maybe CommentInput -> Form CommentInput
newCommentForm mCommentData =  renderBootstrap3 BootstrapBasicForm $ CommentInput
    <$> aopt textareaField preambleSettings (preamble <$> mCommentData)
    <*> areq (selectFieldList inputFormats) "Comment" (inputFormat <$> mCommentData)
    <*> areq textareaField editorSettings (content <$> mCommentData)
    <*> aopt textareaField citationSettings (citation <$> mCommentData)
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

formatDateStr :: UTCTime -> String
formatDateStr t = formatTime defaultTimeLocale "%e %b %Y" t
