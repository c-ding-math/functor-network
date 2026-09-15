{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.UserEntry where

import Import
--import Yesod.Form.Bootstrap3
import Handler.Parse
import Parse.Parser(scaleHeader)
import Handler.Tree(treeWidget)
import Handler.Vote(voteWidget,likeWidget)
--import Handler.Download(downloadWidget)
import Handler.EditComment(getChildIds,newCommentForm,CommentInput(..))
import Handler.NewEntrySubscription(subscribeToEntryWidget)

getUserEntryR :: UserId ->  EntryId -> Handler Html
getUserEntryR authorId entryId = do    
    maybeUserId<-maybeAuthId
    maybeUser<-maybeAuth
    pdfExists<-doesEntryPdfExist entryId

    (maybePayment,entry,entryAuthor,entryCategoryEntities, entryVoteList, commentListData')<-runDB $ do
        entry<-get404 entryId
        maybePayment <- selectFirst [PaymentUserId ==. authorId] [Desc PaymentInserted]
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
            return $ (maybePayment,entry,entryAuthor,entryCategoryEntities,entryVoteList,zip4 commentEntities commentAuthors parentCommentMetas commentVoteLists)       
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

    entryTitleHtml<-entryTitleHtmlCache entryId
    entryBodyHtml<-entryBodyHtmlCache entryId
    commentListData <- forM commentListData' $ \(Entity commentId comment, (commentAuthorId, commentAuthorName), mParentCommentMeta, commentVoteList) -> do
        commentBodyHtml <- entryBodyHtmlCache commentId
        return (Entity commentId comment, (commentAuthorId, commentAuthorName), mParentCommentMeta, commentVoteList, commentBodyHtml)
    defaultLayout $ do
        setTitle $ toHtml $ entryTitle entry

        [whamlet|
<article .entry :entryStatus entry == Draft:.draft #entry-#{toPathPiece entryId}>
  <h1 .entry-title>#{preEscapedToMarkup(scaleHeader 1 entryTitleHtml)}
    $maybe Entity _ payment <- maybePayment
        <a#like-button.btn.btn-primary.pull-right type=button data-action=@{LikeR entryId} href=#{paymentIdent payment} data-toggle=modal data-target=#like-modal>_{MsgLike}
  <div .entry-meta>
    <ul.list-inline>
      <li .by>
        <span title="author"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-person" viewBox="0 0 16 16"><path d="M8 8a3 3 0 1 0 0-6 3 3 0 0 0 0 6m2-3a2 2 0 1 1-4 0 2 2 0 0 1 4 0m4 8c0 1-1 1-1 1H3s-1 0-1-1 1-4 6-4 6 3 6 4m-1-.004c-.001-.246-.154-.986-.832-1.664C11.516 10.68 10.289 10 8 10s-3.516.68-4.168 1.332c-.678.678-.83 1.418-.832 1.664z"/></svg>
        <a .text-muted href=@{UserHomeR (entryUserId entry)}>#{userName entryAuthor}
      <li .at>
        <span title="date"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-clock" viewBox="0 0 16 16"><path d="M8 3.5a.5.5 0 0 0-1 0V9a.5.5 0 0 0 .252.434l3.5 2a.5.5 0 0 0 .496-.868L8 8.71z"/><path d="M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16m7-8A7 7 0 1 1 1 8a7 7 0 0 1 14 0"/></svg>
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

  <div .entry-body>
    <div .entry-content-wrapper>
        $if (entryBody entry) /= Nothing
            #{preEscapedToMarkup entryBodyHtml}
        $else
            <div style="width:519.3906239999999px;">
                <p>_{MsgComingSoon}
  <div .menu>
    <ul.list-inline.text-lowercase>
        <li .reply>
            <a.text-muted href=#comment data-action=@{EditCommentR entryId}>_{MsgComment}
        <li .subscribe>
            <a.text-muted href=# data-action=@{NewEntrySubscriptionR entryId}>_{MsgFollow}
        $if isNothing maybePayment
          <li .vote>
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
                    <a.text-muted href=@{DownloadR authorId entryId}>_{MsgDownload}

        <li .share>
            <a.text-muted href=# data-share-url=@{UserEntryR authorId entryId} data-share-text="#{entryTitle entry}">_{MsgShare}
        $if isAdministrator maybeUserId entry
            <li .edit>
                <a.text-muted href=@{EditUserEntryR entryId}>_{MsgEdit}
<section #comments .comments  :entryStatus entry == Draft:.draft>    
    $if null commentListData
        <p style="display:none">_{MsgNoComment}
    $else
        <h3>_{MsgComments}
        $forall (Entity commentId comment,(commentAuthorId, commentAuthorName),mParentCommentMeta,commentVoteList,commentBodyHtml)<-commentListData
            <article .comment id=entry-#{toPathPiece commentId}> 
                <div .entry-meta>
                  <ul .list-inline>
                    <li .by>
                        <span title="author"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-person" viewBox="0 0 16 16"><path d="M8 8a3 3 0 1 0 0-6 3 3 0 0 0 0 6m2-3a2 2 0 1 1-4 0 2 2 0 0 1 4 0m4 8c0 1-1 1-1 1H3s-1 0-1-1 1-4 6-4 6 3 6 4m-1-.004c-.001-.246-.154-.986-.832-1.664C11.516 10.68 10.289 10 8 10s-3.516.68-4.168 1.332c-.678.678-.83 1.418-.832 1.664z"/></svg>
                        <a .text-muted href=@{UserHomeR commentAuthorId}>#{commentAuthorName}
                    $maybe (parentCommentId, parentCommentAuthorName)<-mParentCommentMeta
                        <li .to>
                            <span title="in reply to"><svg class="bi bi-reply" width="1em" height="1em" viewBox="0 0 16 16" fill="currentColor" xmlns="http://www.w3.org/2000/svg"><path fill-rule="evenodd" d="M9.502 5.013a.144.144 0 0 0-.202.134V6.3a.5.5 0 0 1-.5.5c-.667 0-2.013.005-3.3.822-.984.624-1.99 1.76-2.595 3.876C3.925 10.515 5.09 9.982 6.11 9.7a8.741 8.741 0 0 1 1.921-.306 7.403 7.403 0 0 1 .798.008h.013l.005.001h.001L8.8 9.9l.05-.498a.5.5 0 0 1 .45.498v1.153c0 .108.11.176.202.134l3.984-2.933a.494.494 0 0 1 .042-.028.147.147 0 0 0 0-.252.494.494 0 0 1-.042-.028L9.502 5.013zM8.3 10.386a7.745 7.745 0 0 0-1.923.277c-1.326.368-2.896 1.201-3.94 3.08a.5.5 0 0 1-.933-.305c.464-3.71 1.886-5.662 3.46-6.66 1.245-.79 2.527-.942 3.336-.971v-.66a1.144 1.144 0 0 1 1.767-.96l3.994 2.94a1.147 1.147 0 0 1 0 1.946l-3.994 2.94a1.144 1.144 0 0 1-1.767-.96v-.667z"/></svg>
                            <a .text-muted href=#entry-#{toPathPiece parentCommentId}>#{parentCommentAuthorName}
                    <li .at>
                        <span title="date"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-clock" viewBox="0 0 16 16"><path d="M8 3.5a.5.5 0 0 0-1 0V9a.5.5 0 0 0 .252.434l3.5 2a.5.5 0 0 0 .496-.868L8 8.71z"/><path d="M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16m7-8A7 7 0 1 1 1 8a7 7 0 0 1 14 0"/></svg>
                        #{utcToDate (entryInserted comment)}

                <div .entry-body>
                    <div .entry-content-wrapper>
                        #{preEscapedToMarkup commentBodyHtml}  
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
        when (isJust maybeUserId) $ editorWidget format
        treeWidget entryId
        when (isJust maybeUserId)
            voteWidget 
        likeWidget
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
    addScript $ StaticR js_qrcode_min_js
    toWidget [julius|
        $(document).ready(function() {
            $(".menu .share a, #share-link").click(function(e){
                e.preventDefault();
                let shareUrl=$(this).attr("data-share-url");
                let shareText=encodeURIComponent($(this).attr("data-share-text"));
                //apis: email, twitter, facebook, mathstodon, linkedin, reddit, wordpress, medium, blogger
                let apis =[
                    {api: `mailto:?subject=${shareText}&body=${shareUrl}`, name: "Email", icon: '<svg xmlns="http://www.w3.org/2000/svg" width=20 height=20 fill=currentColor viewBox="0 0 640 640" ><!--!Font Awesome Free v7.3.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2026 Fonticons, Inc.--><path d="M112 128C85.5 128 64 149.5 64 176C64 191.1 71.1 205.3 83.2 214.4L291.2 370.4C308.3 383.2 331.7 383.2 348.8 370.4L556.8 214.4C568.9 205.3 576 191.1 576 176C576 149.5 554.5 128 528 128L112 128zM64 260L64 448C64 483.3 92.7 512 128 512L512 512C547.3 512 576 483.3 576 448L576 260L377.6 408.8C343.5 434.4 296.5 434.4 262.4 408.8L64 260z"/></svg>'},
                    {api: `https://mathstodon.xyz/share?text=${shareText}&url=${shareUrl}`,  name: "Mathstodon", icon: '<svg xmlns="http://www.w3.org/2000/svg" width=20 height=20 fill=currentColor viewBox="0 0 640 640" ><!--!Font Awesome Free v7.3.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2026 Fonticons, Inc.--><path d="M529 243.1C529 145.9 465.3 117.4 465.3 117.4C402.8 88.7 236.7 89 174.8 117.4C174.8 117.4 111.1 145.9 111.1 243.1C111.1 358.8 104.5 502.5 216.7 532.2C257.2 542.9 292 545.2 320 543.6C370.8 540.8 399.3 525.5 399.3 525.5L397.6 488.6C397.6 488.6 361.3 500 320.5 498.7C280.1 497.3 237.5 494.3 230.9 444.7C230.3 440.1 230 435.4 230 430.8C315.6 451.7 388.7 439.9 408.7 437.5C464.8 430.8 513.7 396.2 519.9 364.6C529.7 314.8 528.9 243.1 528.9 243.1zM453.9 368.3L407.3 368.3L407.3 254.1C407.3 204.4 343.3 202.5 343.3 261L343.3 323.5L297 323.5L297 261C297 202.5 233 204.4 233 254.1L233 368.3L186.3 368.3C186.3 246.2 181.1 220.4 204.7 193.3C230.6 164.4 284.5 162.5 308.5 199.4L320.1 218.9L331.7 199.4C355.8 162.3 409.8 164.6 435.5 193.3C459.2 220.6 453.9 246.3 453.9 368.3L453.9 368.3z"/></svg>'},
                    {api: `https://www.reddit.com/submit?title=${shareText}&url=${shareUrl}&type=LINK`,  name: "Reddit", icon: '<svg xmlns="http://www.w3.org/2000/svg" width=20 height=20 fill=currentColor viewBox="0 0 640 640" ><!--!Font Awesome Free v7.3.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2026 Fonticons, Inc.--><path d="M64 320C64 178.6 178.6 64 320 64C461.4 64 576 178.6 576 320C576 461.4 461.4 576 320 576L101.1 576C87.4 576 80.6 559.5 90.2 549.8L139 501C92.7 454.7 64 390.7 64 320zM413.6 217.6C437.2 217.6 456.3 198.5 456.3 174.9C456.3 151.3 437.2 132.2 413.6 132.2C393 132.2 375.8 146.8 371.8 166.2C337.3 169.9 310.4 199.2 310.4 234.6L310.4 234.8C272.9 236.4 238.6 247.1 211.4 263.9C201.3 256.1 188.6 251.4 174.9 251.4C141.9 251.4 115.1 278.2 115.1 311.2C115.1 335.2 129.2 355.8 149.5 365.3C151.5 434.7 227.1 490.5 320.1 490.5C413.1 490.5 488.8 434.6 490.7 365.2C510.9 355.6 524.8 335 524.8 311.2C524.8 278.2 498 251.4 465 251.4C451.3 251.4 438.7 256 428.6 263.8C401.2 246.8 366.5 236.1 328.6 234.7L328.6 234.5C328.6 209.1 347.5 188 372 184.6C376.4 203.4 393.3 217.4 413.5 217.4L413.6 217.6zM241.1 310.9C257.8 310.9 270.6 328.5 269.6 350.2C268.6 371.9 256.1 379.8 239.3 379.8C222.5 379.8 207.9 371 208.9 349.3C209.9 327.6 224.3 311 241 311L241.1 310.9zM431.2 349.2C432.2 370.9 417.5 379.7 400.8 379.7C384.1 379.7 371.5 371.8 370.5 350.1C369.5 328.4 382.3 310.8 399 310.8C415.7 310.8 430.2 327.4 431.1 349.1L431.2 349.2zM383.1 405.9C372.8 430.5 348.5 447.8 320.1 447.8C291.7 447.8 267.4 430.5 257.1 405.9C255.9 403 257.9 399.7 261 399.4C279.4 397.5 299.3 396.5 320.1 396.5C340.9 396.5 360.8 397.5 379.2 399.4C382.3 399.7 384.3 403 383.1 405.9z"/></svg>'},
                    {api: `https://x.com/intent/post?text=${shareText}&url=${shareUrl}`,  name: "X", icon: '<svg xmlns="http://www.w3.org/2000/svg" width=20 height=20 fill=currentColor viewBox="0 0 640 640" ><!--!Font Awesome Free v7.3.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2026 Fonticons, Inc.--><path d="M160 96C124.7 96 96 124.7 96 160L96 480C96 515.3 124.7 544 160 544L480 544C515.3 544 544 515.3 544 480L544 160C544 124.7 515.3 96 480 96L160 96zM457.1 180L353.3 298.6L475.4 460L379.8 460L305 362.1L219.3 460L171.8 460L282.8 333.1L165.7 180L263.7 180L331.4 269.5L409.6 180L457.1 180zM419.3 431.6L249.4 206.9L221.1 206.9L392.9 431.6L419.3 431.6z"/></svg>'},
                    {api: `https://www.blogger.com/blog-this.g?n=${shareText}&u=${shareUrl}`,  name: "Blogger", icon: '<svg xmlns="http://www.w3.org/2000/svg" width=20 height=20 fill=currentColor viewBox="0 0 640 640" ><!--!Font Awesome Free v7.3.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2026 Fonticons, Inc.--><path d="M258.4 260C263.2 255.1 264.6 254.9 294.8 254.9C322 254.9 322.9 255 326.9 257C332.7 259.9 335.2 264 335.2 270.6C335.2 276.5 332.8 280.6 327.6 284C324.8 285.8 323.1 285.9 296.5 286.1C280.1 286.2 267 285.9 265 285.3C254.7 282.4 250.9 267.6 258.4 260zM319.8 354.5C265.9 354.5 264 354.7 259.6 358.6C256.1 361.7 253.9 368 254.5 372.5C255.2 377.2 259.3 382.6 263.7 384.5C265.9 385.5 277.8 386.2 320 385.7L367.9 385.1L377.1 383.6C386.1 378.5 387.6 366.2 380.2 359.2C374.9 354.5 375.2 354.5 319.8 354.5zM543.2 484.6C539.7 513 520.2 535 492.1 542.1C484.9 543.9 482.4 544 319.2 543.9C161.4 543.9 153.3 543.8 147.2 542.1C138.8 539.9 131.6 536.6 124.9 532.1C119.3 528.3 111 520.3 107.9 515.7C104.1 510.1 99.7 500.4 97.9 493.7C96.1 487 96 484.3 96 320.3C96 157.2 96 153.7 97.8 146.6C104.1 121.9 123.7 103 149 97.4C156.3 95.8 481.1 95.5 489 97.1C510.2 101.4 526.9 114.2 536.6 133.5C544.3 148.8 543.6 132 543.9 314.1C544.1 429.9 543.9 478.6 543.2 484.6zM457.8 299.4C456.7 294.4 453.6 289.8 450.1 287.9C449 287.3 442.1 286.6 434.6 286.2C422.2 285.6 420.8 285.4 416.8 283.1C410.6 279.5 408.9 275.5 408.8 264.8C408.8 244.4 400.3 225.4 383.5 208.3C371.5 196.1 358.2 187.8 342.9 183.2C339.3 182.1 331.1 181.7 303.7 181.4C260.8 180.9 251.2 181.8 236.6 187.6C209.6 198.3 190.3 221 183.2 250C181.9 255.4 181.6 264.2 181.3 314.3C180.9 377.1 181.3 386.4 185.3 398.8C195 429.5 222.4 452.2 249.9 457.2C259.1 458.9 372.1 459.3 383.6 457.7C403.7 455 419.5 446.9 434.3 431.8C445 420.9 451.7 409 456.1 393.3C459.3 382.4 459 304.9 457.8 299.4z"/></svg>'},
                    {api: `https://wordpress.com/press-this.php?u=${shareUrl}&t=${shareText}`,  name: "WordPress", icon: '<svg xmlns="http://www.w3.org/2000/svg" width=20 height=20 fill=currentColor viewBox="0 0 640 640" ><!--!Font Awesome Free v7.3.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2026 Fonticons, Inc.--><path d="M125.7 233.4L227.2 511.4C156.2 477 107.3 404.2 107.3 320C107.3 289.1 113.9 259.9 125.7 233.4zM463.6 309.3C463.6 283 454.2 264.8 446.1 250.6C435.3 233.1 425.2 218.2 425.2 200.7C425.2 181.1 440 162.9 460.9 162.9C461.8 162.9 462.7 163 463.7 163.1C425.8 128.4 375.4 107.2 320 107.2C245.7 107.2 180.3 145.3 142.2 203.1C147.2 203.3 151.9 203.4 155.9 203.4C178.1 203.4 212.6 200.7 212.6 200.7C224.1 200 225.4 216.9 214 218.2C214 218.2 202.5 219.5 189.7 220.2L267.2 450.6L313.8 311L280.7 220.2C269.2 219.5 258.4 218.2 258.4 218.2C246.9 217.5 248.3 200 259.7 200.7C259.7 200.7 294.8 203.4 315.7 203.4C337.9 203.4 372.4 200.7 372.4 200.7C383.9 200 385.2 216.9 373.8 218.2C373.8 218.2 362.3 219.5 349.5 220.2L426.4 448.9L447.6 378C456.6 348.6 463.6 327.5 463.6 309.3zM323.7 338.6L259.9 524.1C279 529.7 299.1 532.8 320 532.8C344.8 532.8 368.5 528.5 390.6 520.7C390 519.8 389.5 518.8 389.1 517.8L323.7 338.6zM506.7 217.9C507.6 224.7 508.1 231.9 508.1 239.8C508.1 261.4 504.1 285.6 491.9 316L426.9 503.9C490.2 467 532.7 398.5 532.7 320C532.7 283 523.3 248.2 506.7 217.9zM72 320C72 183 183 72 320 72C457 72 568 183 568 320C568 457 457 568 320 568C183 568 72 457 72 320zM556.6 320C556.6 189.3 450.7 83.4 320 83.4C189.3 83.4 83.4 189.3 83.4 320C83.4 450.7 189.3 556.6 320 556.6C450.7 556.6 556.6 450.7 556.6 320z"/></svg>'}
                ];

                let messages = {
                    shareLink: #{msgRender MsgShareLink},
                    link: #{msgRender MsgLink},
                    qrCode: #{msgRender MsgQRCode},
                    copy: #{msgRender MsgCopy},
                    copied: #{msgRender MsgCopied},
                    shareVia: #{msgRender MsgShareVia},
                    close: #{msgRender MsgClose}
                };
                var modal = dynamicModal({
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
                                    return `<a class="btn" href="${item.api}" target="_blank">${item.icon}</a>`;
                                    }).join(``) + `` + 
                            `<a tabindex="0" role="button" class="btn" data-toggle="popover"><svg xmlns="http://www.w3.org/2000/svg" width=20 height=20 fill=currentColor viewBox="0 0 640 640" ><!--!Font Awesome Free v7.3.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2026 Fonticons, Inc.--><path d="M160 224L224 224L224 160L160 160L160 224zM96 144C96 117.5 117.5 96 144 96L240 96C266.5 96 288 117.5 288 144L288 240C288 266.5 266.5 288 240 288L144 288C117.5 288 96 266.5 96 240L96 144zM160 480L224 480L224 416L160 416L160 480zM96 400C96 373.5 117.5 352 144 352L240 352C266.5 352 288 373.5 288 400L288 496C288 522.5 266.5 544 240 544L144 544C117.5 544 96 522.5 96 496L96 400zM416 160L416 224L480 224L480 160L416 160zM400 96L496 96C522.5 96 544 117.5 544 144L544 240C544 266.5 522.5 288 496 288L400 288C373.5 288 352 266.5 352 240L352 144C352 117.5 373.5 96 400 96zM384 416C366.3 416 352 401.7 352 384C352 366.3 366.3 352 384 352C401.7 352 416 366.3 416 384C416 401.7 401.7 416 384 416zM384 480C401.7 480 416 494.3 416 512C416 529.7 401.7 544 384 544C366.3 544 352 529.7 352 512C352 494.3 366.3 480 384 480zM480 512C480 494.3 494.3 480 512 480C529.7 480 544 494.3 544 512C544 529.7 529.7 544 512 544C494.3 544 480 529.7 480 512zM512 416C494.3 416 480 401.7 480 384C480 366.3 494.3 352 512 352C529.7 352 544 366.3 544 384C544 401.7 529.7 416 512 416zM480 448C480 465.7 465.7 480 448 480C430.3 480 416 465.7 416 448C416 430.3 430.3 416 448 416C465.7 416 480 430.3 480 448z"/></svg></a>` +
                            `</div>` +
                        `</form>`,
                    footer: `<button class='btn btn-default' type='button' data-dismiss="modal">${messages.close}</button>`,
                });   
                let qrCodeSize = 128;
                var qrCodeContainer = $("<div/>", {class:'qrcode-container',height: qrCodeSize, width: qrCodeSize});
                modal.find('a[data-toggle="popover"]').popover({
                    placement: "top",
                    html: true,
                    content: qrCodeContainer,
                    trigger: "focus" 
                });
                modal.find('a[data-toggle="popover"]').on('shown.bs.popover', function () {
                    qrCodeContainer.empty();
                    var qrcode = new QRCode(qrCodeContainer[0], {
                        text: shareUrl,
                        width: qrCodeSize,
                        height: qrCodeSize,
                    });
                });

            });
        });
    |]