{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Users where

import Import
import qualified Data.Set as Set

getUsersR :: Handler Html
getUsersR = do
    -- list users ordered by the amount of entries
    (userListOrderedByPosts, total) <-runDB $ do 
        -- Get all users
        userCount <- count ([]::[Filter User])
        -- Get featured entries for the top users
        featuredEntries <- selectList [EntryFeatured ==. True, EntryType ==. UserPost, EntryStatus ==. Publish] [] 
        let userIds = Set.toList . Set.fromList $ map (entryUserId . entityVal) featuredEntries
        userListWithPostCount <- mapM (\uid -> do 
            postCount <- count [EntryUserId ==. uid, EntryType ==. UserPost, EntryStatus ==. Publish]
            user <- get404 uid
            return (Entity uid user, postCount)) userIds
        return (sortOn (negate . snd) userListWithPostCount, userCount)
    defaultLayout $ do
        setTitleI MsgUsers
        [whamlet|
<div .page-header>
    <h1>_{MsgActiveUsers}
$if null userListOrderedByPosts
    <div>_{MsgNothingFound}
$else 
    <div .entries>
        <p>A total of #{total} users have already joined the platform. Below is a list of active members ranked by the number of posts published.
        <ul.users.list-inline>
            $forall (Entity uid u, m)<-userListOrderedByPosts
                <li>
                    <a.stretched-link href=@{UserHomeR uid}>
                     <div.panel.panel-default>
                      <div.panel-body>  
                        <h4 .entry-title>#{userName u}
                        <span .text-muted>#{m} published posts
                        <p .text-muted>joined on #{utcToDate (userInserted u)}
        |]
