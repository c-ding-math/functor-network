{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.UserEntry where

import Import
--import Yesod.Form.Bootstrap3
import Handler.Parse(editorWidget,doesEntryPdfExist)
import Parse.Parser(scaleHeader)
import Handler.Tree(treeWidget)
import Handler.Vote(voteWidget)
--import Handler.Download(downloadWidget)
import Handler.EditComment(getChildIds,newCommentForm,CommentInput(..))
import Handler.NewEntrySubscription(subscribeToEntryWidget)
--import Data.Text(strip)

getUserEntryR :: UserId ->  EntryId -> Handler Html
getUserEntryR authorId entryId = do    
    maybeUserId<-maybeAuthId
    maybeUser<-maybeAuth
    pdfExists<-doesEntryPdfExist entryId

    (entry,entryAuthor,entryCategoryEntities, entryVoteList, commentListData)<-runDB $ do
        entry<-get404 entryId
        
        if (entryUserId entry==authorId) && (entryStatus entry==Publish || isAdministrator maybeUserId entry) && (entryType entry==UserPost)
          then do
            --mEntryAuthor<- selectFirst [UserId==.entryUserId entry] []
            entryCategoryIds<- map (entryTreeParent . entityVal) <$> selectList [EntryTreeNode ==. entryId] []
            entryCategoryEntities<- selectList [EntryId <-. entryCategoryIds] [Desc EntryInserted]
            entryAuthor<-get404 $ entryUserId entry
            entryVoteList<-selectList [VoteEntryId==.entryId] [Desc VoteInserted]
            commentIds <- getChildIds entryId
            commentEntities <- selectList [EntryId <-. commentIds] [Asc EntryInserted]
            commentAuthors <- mapM (\(Entity _ comment) -> do
                commentAuthor<-get404 $ entryUserId comment
                return (entryUserId comment, userName commentAuthor)
                ) commentEntities
            commentVoteLists <- mapM (\(Entity commentId _) -> selectList [VoteEntryId==.commentId] [Desc VoteInserted]) commentEntities
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
            return $ (entry,entryAuthor,entryCategoryEntities,entryVoteList,zip4 commentEntities commentAuthors parentCommentMetas commentVoteLists)       
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
    <ul.list-inline>
      <li .by>
        <span title="author"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-person" viewBox="0 0 16 16"><path d="M8 8a3 3 0 1 0 0-6 3 3 0 0 0 0 6m2-3a2 2 0 1 1-4 0 2 2 0 0 1 4 0m4 8c0 1-1 1-1 1H3s-1 0-1-1 1-4 6-4 6 3 6 4m-1-.004c-.001-.246-.154-.986-.832-1.664C11.516 10.68 10.289 10 8 10s-3.516.68-4.168 1.332c-.678.678-.83 1.418-.832 1.664z"/></svg><!-- Copyright (c) 2011-2024 The Bootstrap Authors, Licensed under MIT (https://github.com/twbs/bootstrap/blob/main/LICENSE) -->
        <a href=@{UserHomeR (entryUserId entry)}>#{userName entryAuthor}
      <li .at>
        <span title="date"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-clock" viewBox="0 0 16 16"><path d="M8 3.5a.5.5 0 0 0-1 0V9a.5.5 0 0 0 .252.434l3.5 2a.5.5 0 0 0 .496-.868L8 8.71z"/><path d="M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16m7-8A7 7 0 1 1 1 8a7 7 0 0 1 14 0"/></svg><!-- Copyright (c) 2011-2024 The Bootstrap Authors, Licensed under MIT (https://github.com/twbs/bootstrap/blob/main/LICENSE) -->
        $if entryInserted entry == entryUpdated entry
            #{utcToDate (entryInserted entry)}
        $else
            <span .dropdown>
                <a .text-muted.dropdown-toggle data-toggle=dropdown>
                    #{utcToDate (entryInserted entry)}
                    <span .caret>
                <ul .dropdown-menu>
                    <li>
                        <a .text-muted onclick="return false;" href=#>
                            created #{utcToDateTime (entryInserted entry)}
                    <li>
                        <a .text-muted onclick="return false;" href=#>
                            updated #{utcToDateTime (entryUpdated entry)}

      $if not (null entryCategoryEntities)
        <li .in>
            <span title="category"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-folder" viewBox="0 0 16 16"><path d="M.54 3.87.5 3a2 2 0 0 1 2-2h3.672a2 2 0 0 1 1.414.586l.828.828A2 2 0 0 0 9.828 3h3.982a2 2 0 0 1 1.992 2.181l-.637 7A2 2 0 0 1 13.174 14H2.826a2 2 0 0 1-1.991-1.819l-.637-7a2 2 0 0 1 .342-1.31zM2.19 4a1 1 0 0 0-.996 1.09l.637 7a1 1 0 0 0 .995.91h10.348a1 1 0 0 0 .995-.91l.637-7A1 1 0 0 0 13.81 4zm4.69-1.707A1 1 0 0 0 6.172 2H2.5a1 1 0 0 0-1 .981l.006.139q.323-.119.684-.12h5.396z"/></svg><!-- Copyright (c) 2011-2024 The Bootstrap Authors, Licensed under MIT (https://github.com/twbs/bootstrap/blob/main/LICENSE) -->
            <span .dropdown>
                <a .text-muted.dropdown-toggle data-toggle=dropdown>
                    #{show $ length entryCategoryEntities} #
                    $if length entryCategoryEntities <= 1
                        category 
                    $else
                        categories 
                    <span .caret>
                <ul .dropdown-menu>
                    $forall (Entity categoryId category)<-entryCategoryEntities
                        <li>
                            <a href=@{CategoriesR (entryUserId category)}#{"#entry-" <> toPathPiece categoryId}>#{preEscapedToMarkup $ entryTitleHtml category}
  <div .entry-content>
      <div .entry-content-wrapper>
        $if (entryBody entry) /= Nothing
            #{preEscapedToMarkup(entryBodyHtml entry)}
        $else
            <div style="width:519.3906239999999px;">
                <p>_{MsgComingSoon}
  <div .menu>
    <ul.list-inline.text-lowercase>
        <li .reply>
            <a.text-muted href=#comment data-action=@{EditCommentR entryId}>_{MsgComment}
        <li .subscribe>
            <a.text-muted href=# data-action=@{NewEntrySubscriptionR entryId}>_{MsgFollow}
        <li .vote.hidden>
            $maybe userId<-maybeUserId
                <a.text-muted href=# data-action=@{VoteR entryId}>
                    $if (elem userId (map (voteUserId . entityVal) entryVoteList))
                        _{MsgLiked}
                    $else
                        _{MsgLike}
            $nothing
                <a.text-muted href=@{AuthR LoginR}>_{MsgLike}
            <span .text-muted>
                <span.badge style="color:inherit;background-color:inherit;border:1px solid;" :null entryVoteList:.hidden>#{show $ length entryVoteList}
        <li .categorize>
            <a.text-muted href=# data-action=@{TreeR entryId}>_{MsgCategorize}
        $if pdfExists
            <li .download>
                $maybe _ <-maybeUserId
                    <a.text-muted href=@{DownloadR authorId entryId}>_{MsgDownload}
                $nothing
                    <a.text-muted href=@{AuthR LoginR}>_{MsgDownload}
        <li .share>
            <a.text-muted href=# data-link=@{UserEntryR authorId entryId}>_{MsgShare}
        $if isAdministrator maybeUserId entry
            <li .edit>
                <a.text-muted href=@{EditUserEntryR entryId}>_{MsgEdit}
<section #comments .comments  :entryStatus entry == Draft:.draft>    
    $if null commentListData
        <p style="display:none">_{MsgNoComment}
    $else
        <h3>_{MsgComments}
        $forall (Entity commentId comment,(commentAuthorId, commentAuthorName),mParentCommentMeta,commentVoteList)<-commentListData
            <article .comment id=entry-#{toPathPiece commentId}> 
                <div .entry-meta>
                  <ul .list-inline>
                    <li .by>
                        <span title="author"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-person" viewBox="0 0 16 16"><path d="M8 8a3 3 0 1 0 0-6 3 3 0 0 0 0 6m2-3a2 2 0 1 1-4 0 2 2 0 0 1 4 0m4 8c0 1-1 1-1 1H3s-1 0-1-1 1-4 6-4 6 3 6 4m-1-.004c-.001-.246-.154-.986-.832-1.664C11.516 10.68 10.289 10 8 10s-3.516.68-4.168 1.332c-.678.678-.83 1.418-.832 1.664z"/></svg><!-- Copyright (c) 2011-2024 The Bootstrap Authors, Licensed under MIT (https://github.com/twbs/bootstrap/blob/main/LICENSE) -->
                        <a href=@{UserHomeR commentAuthorId}>#{commentAuthorName}
                    $maybe (parentCommentId, parentCommentAuthorName)<-mParentCommentMeta
                        <li .to>
                            <span title="in reply to"><svg class="bi bi-reply" width="1em" height="1em" viewBox="0 0 16 16" fill="currentColor" xmlns="http://www.w3.org/2000/svg"><path fill-rule="evenodd" d="M9.502 5.013a.144.144 0 0 0-.202.134V6.3a.5.5 0 0 1-.5.5c-.667 0-2.013.005-3.3.822-.984.624-1.99 1.76-2.595 3.876C3.925 10.515 5.09 9.982 6.11 9.7a8.741 8.741 0 0 1 1.921-.306 7.403 7.403 0 0 1 .798.008h.013l.005.001h.001L8.8 9.9l.05-.498a.5.5 0 0 1 .45.498v1.153c0 .108.11.176.202.134l3.984-2.933a.494.494 0 0 1 .042-.028.147.147 0 0 0 0-.252.494.494 0 0 1-.042-.028L9.502 5.013zM8.3 10.386a7.745 7.745 0 0 0-1.923.277c-1.326.368-2.896 1.201-3.94 3.08a.5.5 0 0 1-.933-.305c.464-3.71 1.886-5.662 3.46-6.66 1.245-.79 2.527-.942 3.336-.971v-.66a1.144 1.144 0 0 1 1.767-.96l3.994 2.94a1.147 1.147 0 0 1 0 1.946l-3.994 2.94a1.144 1.144 0 0 1-1.767-.96v-.667z"/></svg><!-- Copyright (c) 2011-2024 The Bootstrap Authors, Licensed under MIT (https://github.com/twbs/bootstrap/blob/main/LICENSE) -->
                            <a href=#entry-#{toPathPiece parentCommentId}>#{parentCommentAuthorName}
                    <li .at>
                        <span title="date"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-clock" viewBox="0 0 16 16"><path d="M8 3.5a.5.5 0 0 0-1 0V9a.5.5 0 0 0 .252.434l3.5 2a.5.5 0 0 0 .496-.868L8 8.71z"/><path d="M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16m7-8A7 7 0 1 1 1 8a7 7 0 0 1 14 0"/></svg><!-- Copyright (c) 2011-2024 The Bootstrap Authors, Licensed under MIT (https://github.com/twbs/bootstrap/blob/main/LICENSE) -->
                        #{utcToDate (entryInserted comment)}

                <div .entry-content>
                    <div .entry-content-wrapper>#{preEscapedToMarkup (entryBodyHtml comment)}  
                <div.menu>
                    <ul.list-inline.text-lowercase>                
                        <li .reply>
                            <a.text-muted href=#comment data-action=@{EditCommentR commentId}>reply
                        <li .subscribe>
                            <a.text-muted href=# data-action=@{NewEntrySubscriptionR commentId}>_{MsgFollow}
                        <li .vote>
                            $maybe userId<-maybeUserId
                                <a.text-muted href=# data-action=@{VoteR commentId}>
                                    $if (elem userId (map (voteUserId . entityVal) commentVoteList))
                                        _{MsgVoted}
                                    $else
                                        _{MsgVote}
                            $nothing
                                <a.text-muted href=@{AuthR LoginR}>_{MsgVote}
                            <span .text-muted>
                                <span.badge style="color:inherit;background-color:inherit;border:1px solid;" :null commentVoteList:.hidden>#{show $ length commentVoteList}
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
        treeWidget entryId
        when (isJust maybeUserId)
            voteWidget 
        menuWidget
        shareWidget
        --downloadWidget entryId

        
menuWidget::Widget
menuWidget=do
    subscribeToEntryWidget
    msgRender<-getMessageRender
    toWidget [julius|
        $(".new-comment form button").click(function(){
            $(".new-comment form").submit();   
        });
        $(document).ready(function()	{
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


shareWidget :: Widget
shareWidget = do
    msgRender <- getMessageRender
    toWidget [julius|
        $(document).ready(function() {
            $(".menu .share a").click(function(e){
                e.preventDefault();
                //apis: email, twitter, facebook, mathstodon, bluesky, reddit, wordpress, blogger
                let apis =[
                    {api: "mailto:?body=", name: "Email", icon: "@{StaticR icons_envelope_svg}"},
                    {api: "https://mathstodon.xyz/share?text=",  name: "Mathstodon", icon: "@{StaticR icons_mathstodon_logo_svg}"},
                    {api: "https://www.reddit.com/submit?text=",  name: "Reddit", icon: "@{StaticR icons_reddit_logo_svg}"},
                    {api: "https://x.com/intent/post?text=",  name: "X", icon: "@{StaticR icons_twitter_logo_svg}"},
                ];
                let shareUrl=$(this).attr("data-link");
                let messages = {
                    shareLink: #{msgRender MsgShareLink},
                    link: #{msgRender MsgLink},
                    copy: #{msgRender MsgCopy},
                    copied: #{msgRender MsgCopied},
                    shareVia: #{msgRender MsgShareVia},
                    close: #{msgRender MsgClose}
                };
                dynamicModal({
                    header: `<b>${messages.shareLink}</b>`,
                    body: `
                        <form>
                            <label class="control-label">${messages.link}</label>
                            <div class="input-group">
                                <input type="text" class="form-control" value="${shareUrl}" readonly>
                                <div class="input-group-btn"><button type="button" class="btn btn-primary" onclick="navigator.clipboard.writeText('${shareUrl}');that=$(this);that.html('${messages.copied}');setTimeout(function(){that.html('${messages.copy}');}, 2000);">${messages.copy}</button></div>
                            </div>
                            <hr>
                            <label>${messages.shareVia}</label>` +
                            `<div class="share-buttons">`+ 
                            apis.map(function(item) {
                                    return `<a class="btn btn-secondary" href="${item.api}${shareUrl}" target="_blank"><img src="${item.icon}" alt="${item.name}"></a>`;
                                    }).join(``) +
                            `</div>` +
                        `</form>`,
                    footer: `<button class='btn btn-default' type='button' data-dismiss="modal">${messages.close}</button>`,
                });

            });
        });
    |]