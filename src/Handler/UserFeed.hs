{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.UserFeed where

import Import
--import Yesod.Feed

getUserFeedR :: UserId -> Handler TypedContent
getUserFeedR authorId = do
    
    (author,posts) <- runDB $ do
        author <- get404 authorId
        posts <- selectList [EntryUserId ==. authorId, EntryType ==. UserPost, EntryStatus ==. Publish] [Desc EntryInserted, LimitTo 20]
        --entry <- selectFirst [EntryUserId ==. authorId, EntryType ==. UserPost, EntryStatus ==. Publish] [Desc EntryInserted, LimitTo 20]
        return (author,posts)
    
    --urlRender <- getUrlRender

    -- Define the main feed information.
    --h<-defaultLayout [whamlet|#{preEscapedToMarkup(entryBodyHtml entry)}|] 
    let feedInfo = Feed
            { feedTitle       = userName author
            , feedLinkSelf    = UserFeedR authorId
            , feedLinkHome    = UserEntriesR authorId
            , feedAuthor      = userName author
            , feedDescription = toHtml $ "The latest posts by " <> userName author
            , feedLanguage    = "en"
            , feedUpdated     = case posts of
                                  (Entity _ post : _) -> entryInserted post
                                  []                     -> userInserted author
            , feedLogo        = Nothing -- Maybe (url, Text)
            , feedEntries     = map toFeedEntry posts
            }

    --  Generate and serve the feed as RSS 2.0.
    --  For an Atom 1.0 feed, use `atomFeed` instead.
    newsFeed feedInfo

-- Helper function to convert a Entry into a FeedEntry.
toFeedEntry :: Entity Entry -> FeedEntry (Route App)
toFeedEntry (Entity entryId entry) = FeedEntry
    { feedEntryTitle    = entryTitle entry
    , feedEntryLink     = UserEntryR (entryUserId entry) entryId -- Assumes an UserEntryR (entryUserId entry) route exists
    , feedEntryContent  = toHtml $ case entryBody entry of Just body -> take 512 (unTextarea body) <> "[...]"; Nothing -> "comming soon..."
    , feedEntryUpdated  = entryInserted entry
    , feedEntryEnclosure= Nothing
    , feedEntryCategories=[]
    }
