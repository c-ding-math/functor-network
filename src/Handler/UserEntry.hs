{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.UserEntry where

import Import
--import Yesod.Form.Bootstrap3
import Handler.Parser(markItUpWidget)
import Parse.Parser(scaleHeader)
import Handler.EditComment(getChildIds,newCommentForm,CommentInput(..))
import Handler.NewEntrySubscription(subscribeToEntryWidget)
import Data.Text(strip)


getUserEntryR :: UserId ->  EntryId -> Handler Html
getUserEntryR authorId entryId = do    
    maybeUserId<-maybeAuthId
    maybeUser<-maybeAuth
    (entry,mEntryAuthor,comments,mCommentAuthors,mCommentParentIds,mCommentParentAuthors)<-runDB $ do
        entry<-get404 entryId
        
        if (entryUserId entry==authorId) && (entryStatus entry==Publish || isAdministrator maybeUserId entry) && (entryType entry==UserPost)
          then do
            mEntryAuthor<- selectFirst [UserId==.entryUserId entry] []
            commentIds<- getChildIds entryId
            comments<-selectList [EntryId <-. commentIds, EntryStatus==.Publish, EntryType==.Comment][Asc EntryInserted]     
            --commentHtmls<-mapM (\x->getBy404 $ UniqueEntryHtml $ entityKey x) comments
            mCommentAuthors<-mapM (\x-> selectFirst [UserId==.entryUserId (entityVal x)] []) comments
            mCommentParentIds<-mapM (\comment -> do
                mEntryTree<-selectFirst [EntryTreeNode==.entityKey comment] [] 
                case mEntryTree of
                    Just tree -> return $ Just $ entryTreeParent $ entityVal tree
                    Nothing -> return Nothing
                ) comments

            mCommentParentAuthors<-mapM (\mCommentParentId -> do
                case mCommentParentId of
                    Just parentId -> do
                        mParentComment<-get parentId
                        case mParentComment of
                            Just parentComment -> do
                                mCommentParentAuthor<- selectFirst [UserId==.entryUserId parentComment] []
                                return mCommentParentAuthor
                            Nothing -> return Nothing
                    Nothing -> return Nothing
                ) mCommentParentIds
            return $ (entry,mEntryAuthor,comments,mCommentAuthors,mCommentParentIds,mCommentParentAuthors)        
          else notFound

    formatParam <- lookupGetParam "format"
    let format = case formatParam of
            Just "tex" -> Format "tex"
            Just "md" -> Format "md"
            _-> case maybeUser of
                    Just (Entity _ user) -> userDefaultFormat user
                    Nothing -> Format "md"
    (commentWidget, commentEnctype) <- case maybeUser of
        Nothing ->  generateFormPost $ newCommentForm Nothing
        Just (Entity _ user) -> generateFormPost $ newCommentForm $ Just $ CommentInput (userDefaultPreamble user) format (Textarea "") (userDefaultCitation user)

    defaultLayout $ do
        setTitle $ toHtml $ entryTitle entry

        [whamlet|
<article .entry :entryStatus entry == Draft:.draft #entry-#{toPathPiece entryId}>
  <h1 .entry-title>#{preEscapedToMarkup(scaleHeader 1 (entryTitleHtml entry))}
  <div .entry-meta>
      <span .by>
          $maybe author<-mEntryAuthor      
              <a href=#{userAbout (entityVal author)}>#{userName $ entityVal author}
          $nothing 
              _{MsgUnregisteredUser}
      <span .at>#{formatDateStr (entryInserted entry)}
  <div .entry-content>
      <div .entry-content-wrapper>
        $if strip (entryBodyHtml entry) /= "" 
            #{preEscapedToMarkup(entryBodyHtml entry)}
        $else
            _{MsgComingSoon}
  <ul .entry-menu>
        <li .reply>
            <a href=#comment data-action=@{EditCommentR entryId}>comment
        <!--<li .print><a href=#>print</a>-->
        <li .subscribe>
            <a href=# data-action=@{NewEntrySubscriptionR entryId}>_{MsgFollow}     
        $if isAdministrator maybeUserId entry
            <li .edit><a href=@{EditUserEntryR entryId}>edit</a>

<section #comments .comments  :entryStatus entry == Draft:.draft>    
    $if null comments
        <p style="display:none">_{MsgNoComment}
    $else
        <h3>_{MsgComments}
        $forall (Entity commentId comment,mCommentAuthor,mCommentParentId,mCommentParentAuthor)<-zip4 comments mCommentAuthors mCommentParentIds mCommentParentAuthors
            <article .comment id=entry-#{toPathPiece commentId}> 
              <div .entry-meta>
                  <span .by>
                      $maybe author<-mCommentAuthor                           
                          <a href=#{userAbout (entityVal author)}>#{userName $ entityVal author}
                      $nothing 
                          _{MsgUnregisteredUser}
                  <span .at>#{formatDateStr (entryInserted comment)}
                  $maybe parentCommentId<-mCommentParentId
                    $if parentCommentId /= entryId
                        <span .to>
                            <a href=#entry-#{toPathPiece parentCommentId}>
                                $maybe parentAuthor<-mCommentParentAuthor
                                    #{userName $ entityVal parentAuthor}
                                $nothing
                                    _{MsgUnregisteredUser}

              <div .entry-content>
                  <div .entry-content-wrapper>#{preEscapedToMarkup (entryBodyHtml comment)}  
              <ul .entry-menu>                
                  <li .reply>
                    <a href=#comment data-action=@{EditCommentR commentId}>reply
                  <li .subscribe>
                    <a href=# data-action=@{NewEntrySubscriptionR commentId}>_{MsgFollow}
                  $if isAdministrator maybeUserId entry || isAdministrator maybeUserId comment
                      <li .delete>
                        <a href=@{EditCommentR commentId}>_{MsgDelete}

<section .new-comment>
        $maybe _ <- maybeUser
            <h3 #comment>_{MsgNewComment}
            <form method=post enctype=#{commentEnctype} action=@{EditCommentR entryId}>

                ^{commentWidget}
                <div>
                    <button .btn.btn-primary type=submit>_{MsgComment}
        $nothing
            <h3 #comment>_{MsgNewComment}
            <p>
                You must <a href=@{AuthR LoginR}>log in</a> to post a comment.
                
        |]
        markItUpWidget format (Format "html")
        subscribeToEntryWidget entryId
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


formatDateStr :: UTCTime -> String
formatDateStr t = formatTime defaultTimeLocale "%e %b %Y" t
