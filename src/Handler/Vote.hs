{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Vote where

import Import

postVoteR :: EntryId -> Handler Value
postVoteR entryId = do
    userId <- requireAuthId
    msgRender <- getMessageRender
    let voteText entry = msgRender $ if entryType entry `elem` [Post, Page, UserPost, UserPage, Category] then MsgLike else MsgVote
        votedText entry = msgRender $ if entryType entry `elem` [Post, Page, UserPost, UserPage, Category] then MsgLiked else MsgVoted
    runDB $ do
        currentTime <- liftIO getCurrentTime
        entry <- get404 entryId
        eVote <- insertBy $ Vote userId entryId currentTime
        case eVote of
            Left (Entity key _) -> do
                delete key
                selectList [VoteEntryId ==. entryId] [Desc VoteInserted] >>= (\votes -> return $ object ["text" .= voteText entry, "badge" .= length votes])
            Right _ -> selectList [VoteEntryId ==. entryId] [Desc VoteInserted] >>= (\votes -> return $ object ["text" .= votedText entry, "badge" .= length votes])

voteWidget :: Widget
voteWidget = do
    mcsrftoken <- fmap reqToken getRequest                                                                                                                   
    let csrftoken = case mcsrftoken of                                                                                                                      
                        Nothing -> "NO_TOKEN"                                                                                                               
                        Just t  -> t
    toWidget [julius|
        $(document).ready(function() {
            $('.vote a').click(function() {
                var that = $(this);
                var url = $(this).data('action');
                $.ajax({
                    url: url,
                    type: 'POST',
                    beforeSend: function(xhr) {
                        xhr.setRequestHeader("X-CSRF-Token", #{csrftoken});
                    },
                    success: function(data) {
                        that.text(data.text);
                        var badge = that.parent().find('.badge');
                        badge.text(data.badge);
                        if (data.badge > 0) {
                            badge.removeClass('hidden');
                        } else {
                            badge.addClass('hidden');
                        }

                    }
                });
                return false;
            });
        });
    |]


postLikeR :: EntryId -> Handler Value
postLikeR entryId = do
    urlRender <- getUrlRender
    mUserId <- liftHandler maybeAuthId
    --msgRender <- getMessageRender
    like <- runInputPost $ ireq boolField "like"
    
    runDB $ do
        currentTime <- liftIO getCurrentTime
        --entry <- get404 entryId
        votes <- selectList [VoteEntryId ==. entryId] [Desc VoteInserted]
        voters <- selectList [UserId <-. map (voteUserId .  entityVal) votes] []
        
        case mUserId of
            Just userId -> do
                let voters' = filter (\x -> entityKey x /= userId) voters
                eVote <- insertBy $ Vote userId entryId currentTime
                if like 
                    then do
                        let message :: Text
                            message = if elem userId (map entityKey voters)
                                        then "You've already given this post a like. " <> (if null voters' then "Great taste! " else "Here are the others who did too: ")
                                        else (if null voters then "Eagle eyes! First like is yours. " else "Great taste! Here are the others who like this post too: ")
                        return $ object ["message" .= message, "voters" .= map (\x-> (((userName .entityVal) x, urlRender (UserHomeR (entityKey x))))) voters']
                    else 
                        case eVote of
                            Left (Entity key _) -> do
                                delete key
                                return $ object []
                            Right key -> do
                                delete key
                                return $ object []
            _ -> do
                let voterNames = map (userName . entityVal) voters
                let message :: Text
                    message = if null voters
                        then "First like! Log in to give your like, or support the author by making a donation."
                        else "Nice taste! Log in to give a like, or support the author by making a donation. Here are the others who like this post too: "
                return $ object ["message" .= message, "voters" .= map (\x-> ((userName .entityVal) x, urlRender (UserHomeR (entityKey x)))) voters]

likeWidget :: Widget
likeWidget = do
    maybeUserId <- maybeAuthId
    [whamlet|
<div#like-modal .modal.fade>
    <div .modal-dialog>
        <div .modal-content>
            <div .modal-header>
                <button type=button .close data-dismiss=modal>&times;
                <b .modal-title>_{MsgLike}
            <div .modal-body>
                <p.like-message>
                <ul.voters.list-inline>
                <p>
            <div .modal-footer>
                        <a .btn.btn-default type=button href=# data-dismiss=modal>_{MsgContinueReading}
                        $if isJust maybeUserId
                            <a#unlike-button.btn.btn-default type=button href=# data-dismiss=modal>_{MsgUnLike}
                        $else
                            <a .btn.btn-default type=button href=@{AuthR LoginR} data-dismiss=modal>_{MsgLogIn}
                        <a#donation-button .btn.btn-primary type=button target=_blank href=# data-dismiss=modal>_{MsgSupportAuthor}

    |]
    toWidget [julius|
        (function() {
            $('#donation-button').click(function() {
                window.open($('#like-button').attr('href'));
            });
            var like = true;
            $('#like-button').click(function() {
                like = true;
            });
            $('#unlike-button').click(function() {
                like = false;
            });
            $('#like-button, #unlike-button').click(function() {
                var that = $(this);
                $.ajax({
                    url: $('#like-button ').data('action'),
                    type: 'POST',
                    data: {like: like},
                    dataType: "json",
                    beforeSend: function(xhr) {
                        
                    },
                    success: function(data) {
                        let message = data.message;
                        let voters = data.voters;
                        $('#like-modal p.like-message').text(message);
                        $('#like-modal ul.voters').empty();
                        for (let i = 0; i < voters.length; i++) {
                            $('.voters').append(($('<li>').append($('<a/>',{href: voters[i][1], html: voters[i][0], target: '_blank'}))));
                        }
                        $('#like-modal ul.voters>li:not(:last-child)').append(',');
                        $('#like-modal ul.voters>li:last-child').append('.');

                    }
                });
                //return false;
            });
        })();
    |]
