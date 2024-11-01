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