{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Users where

import Import

getUsersR :: Handler Html
getUsersR = do
    -- list users ordered by the amount of entries
    let topAmount = 100::Int
    (total,userListOrderedByEntries) <- runDB $ do
        allUsers<- selectList [] [Asc UserInserted]
        userListWithEntries <- mapM (\(Entity uid user) -> do
            entries <- selectList [EntryUserId==.uid,EntryStatus==.Publish,EntryType==.UserPost] [Desc EntryInserted]
            let featuredEntries = filter (\(Entity _ e) -> entryFeatured e) entries
            return (Entity uid user, length entries, length featuredEntries)) allUsers
        return (length allUsers, take topAmount $ sortBy (comparing (\(_,m,n)->(Down m,Down n))) userListWithEntries)
        
    defaultLayout $ do
        setTitleI MsgUsers
        [whamlet|
<div .page-header>
    <h1>_{MsgActiveUsers}
$if null userListOrderedByEntries
    <div>_{MsgNothingFound}
$else 
    <div .entries>
        <p>A total of #{total} users have already joined the platform. Below is a list of the top 100 users ranked by the number of posts published.
        <ul .users.list-inline>
            $forall (Entity uid u, m, _)<-userListOrderedByEntries
                <li style="width:12em; margin-right:0.5em;">
                    <a href=@{UserHomeR uid}>
                        <h4>#{userName u}
                    <span .text-muted>#{m} published posts
                    <p .text-muted>registered #{utcToDate (userInserted u)}
        |]
