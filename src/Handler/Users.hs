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
            return (Entity uid user, length entries)) allUsers
        return (length allUsers, take topAmount $ reverse $ sortOn snd userListWithEntries)
        
    defaultLayout $ do
        setTitleI MsgUsers
        [whamlet|
<div .page-header>
    <h1>_{MsgActiveUsers}
$if null userListOrderedByEntries
    <div>_{MsgNothingFound}
$else 
    <div .entries>
        <p>#{total} people have already registered on the platform.  Here is the list of the top #{topAmount} users ordered by the amount of published posts:
        <ul .users.list-inline>
            $forall (Entity uid u, n)<-userListOrderedByEntries
                <li style="width:12em; margin-right:0.5em;">
                    <a href=@{UserHomeR uid}>
                        <h4>#{userName u}
                    <span .text-muted>#{n} published posts
                    <p .text-muted>registered #{utcToDate (userInserted u)}
        |]
