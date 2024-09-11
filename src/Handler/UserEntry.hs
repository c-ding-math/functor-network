{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.UserEntry where

import Import
--import Yesod.Form.Bootstrap3
import Handler.Parser(editorWidget)
import Parse.Parser(scaleHeader)
import Handler.EditComment(getChildIds,newCommentForm,CommentInput(..))
import Handler.NewEntrySubscription(subscribeToEntryWidget)
import Data.Text(strip)


getUserEntryR :: UserId ->  EntryId -> Handler Html
getUserEntryR authorId entryId = do    
    maybeUserId<-maybeAuthId
    maybeUser<-maybeAuth
    (entry,entryAuthor,commentListData)<-runDB $ do
        entry<-get404 entryId
        
        if (entryUserId entry==authorId) && (entryStatus entry==Publish || isAdministrator maybeUserId entry) && (entryType entry==UserPost)
          then do
            --mEntryAuthor<- selectFirst [UserId==.entryUserId entry] []
            entryAuthor<-get404 $ entryUserId entry
            commentIds <- getChildIds entryId
            commentEntities <- selectList [EntryId <-. commentIds] [Asc EntryInserted]
            commentAuthors <- mapM (\(Entity _ comment) -> do
                commentAuthor<-get404 $ entryUserId comment
                return (entryUserId comment, userName commentAuthor)
                ) commentEntities
            parentCommentMetas <- mapM (\commentEntity -> do
                mEntryTree <- selectFirst [EntryTreeNode==.entityKey commentEntity] []
                case mEntryTree of
                    Just tree -> do
                        let parentCommentId = entryTreeParent $ entityVal tree
                        if parentCommentId == entryId
                            then return Nothing
                            else do
                                parentComment <- get404 parentCommentId
                                parentCommentAuthor <- get404 $ entryUserId parentComment
                                return $ Just (parentCommentId, userName parentCommentAuthor)
                    _ -> return Nothing
                ) commentEntities
            return $ (entry,entryAuthor,zip3 commentEntities commentAuthors parentCommentMetas)       
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
        <a href=@{UserHomeR (entryUserId entry)}>#{userName entryAuthor}
      <span .at>#{formatDateStr (entryInserted entry)}
  <div .entry-content>
      <div .entry-content-wrapper>
        $if strip (entryBodyHtml entry) /= "" 
            #{preEscapedToMarkup(entryBodyHtml entry)}
        $else
            _{MsgComingSoon}
  <div .menu>
    <ul.list-inline.text-lowercase>
        <li .reply>
            <a.text-muted href=#comment data-action=@{EditCommentR entryId}>_{MsgComment}
        <!--<li .print><a.text-muted href=#>print</a>-->
        <li .subscribe>
            <a.text-muted href=# data-action=@{NewEntrySubscriptionR entryId}>_{MsgFollow}     
        $if isAdministrator maybeUserId entry
            <li .edit>
                <a.text-muted href=@{EditUserEntryR entryId}>_{MsgEdit}
<section #comments .comments  :entryStatus entry == Draft:.draft>    
    $if null commentListData
        <p style="display:none">_{MsgNoComment}
    $else
        <h3>_{MsgComments}
        $forall (Entity commentId comment,(commentAuthorId, commentAuthorName),mParentCommentMeta)<-commentListData
            <article .comment id=entry-#{toPathPiece commentId}> 
                <div .entry-meta>
                    <span .by>                  
                        <a href=@{UserHomeR commentAuthorId}>#{commentAuthorName}
                    $maybe (parentCommentId, parentCommentAuthorName)<-mParentCommentMeta
                        <span .to>
                            <svg style="height:1em;vertical-align:middle;" class="bi bi-reply-fill" width="1em" height="1em" viewBox="0 0 16 16" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
                                <path d="M9.079 11.9l4.568-3.281a.719.719 0 0 0 0-1.238L9.079 4.1A.716.716 0 0 0 8 4.719V6c-1.5 0-6 0-7 8 2.5-4.5 7-4 7-4v1.281c0 .56.606.898 1.079.62z"/>
                            <a href=#entry-#{toPathPiece parentCommentId}>
                                #{parentCommentAuthorName}
                    <span .at>#{formatDateStr (entryInserted comment)}

                <div .entry-content>
                    <div .entry-content-wrapper>#{preEscapedToMarkup (entryBodyHtml comment)}  
                <div.menu>
                    <ul.list-inline.text-lowercase>                
                        <li .reply>
                            <a.text-muted href=#comment data-action=@{EditCommentR commentId}>reply
                        <li .subscribe>
                            <a.text-muted href=# data-action=@{NewEntrySubscriptionR commentId}>_{MsgFollow}
                        $if isAdministrator maybeUserId entry || isAdministrator maybeUserId comment
                            <li .delete>
                                <a.text-muted href=@{EditCommentR commentId}>_{MsgDelete}

<section .new-comment>
        $maybe _ <- maybeUser
            <h3 #comment>_{MsgNewComment}
            <form method=post enctype=#{commentEnctype} action=@{EditCommentR entryId}>
                ^{commentWidget}
                <div .text-left>
                    <button .btn.btn-primary type=button>_{MsgComment}
        $nothing
            <h3 #comment>_{MsgNewComment}
            <p>
                You must <a href=@{AuthR LoginR}>log in</a> to post a comment.
                
        |]
        editorWidget format
        menuWidget

        
menuWidget::Widget
menuWidget=do
    subscribeToEntryWidget
    msgRender<-getMessageRender
    toWidget [julius|
        $(".new-comment form button").click(function(){
            $(".new-comment form").submit();   
        });
        $(document).ready(function()	{
            /*$(".menu .print a").click(function(){
                window.print();
                return false;
            }); 
            $(".menu .favorite a,.comment .vote a").click(function(){
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

            $(".entry .menu .reply a").click(function(){
                $("#comment").html(commentLabel);
                var handlerUrl=$(this).attr("data-action");
                $(".new-comment form").attr("action", handlerUrl);
            });
            var commentLabel=$("#comment").html();
            $(".comment .menu .reply a").click(function(){
                var parent=$(this).parent();
                if (parent.hasClass("ing")){
                    parent.removeClass("ing");
                    $(this).html("reply");
                    $(".entry .menu .reply a").click();
                } else {
                    $(".reply.ing").removeClass("ing").find("a").html("reply");
                    parent.addClass("ing");
                    $(this).html("cancel reply");
                    var handlerUrl=$(this).attr("data-action");
                    $(".new-comment form").attr("action", handlerUrl);
                    var parentId=$(this).closest(".comment").attr("id").replace("entry-","");
                    var parentCommentAuthor=$(this).closest(".comment").find(".entry-meta .by a").html();
                    $("#comment").html("Reply to <a href=#entry-"+parentId+">"+parentCommentAuthor+"</a>");
                }
            });
            $(".menu .delete a").click(function(event){  
                if (confirm(#{msgRender MsgCommentDeletionConfirmation})) {
            
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
