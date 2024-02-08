{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Users where

import Import

getUsersR :: Handler Html
getUsersR = do
    -- list users ordered by the amount of entries
    
    allUsers<- runDB $ selectList [] [Asc UserInserted]
    users <- runDB $ filterM (\(Entity uid _) -> do
        entries <- selectList [EntryUserId==.uid,EntryStatus==.Publish] [Desc EntryInserted]
        return $ not (null entries)) allUsers
    
    defaultLayout $ do
        setTitleI MsgUsers
        [whamlet|
<h1>_{MsgActiveUsers}
$if null users
    <div>_{MsgNothingFound}
$else 
    <div .entries>
        <p>#{length allUsers} people have registered on our platform. Here is a list of members who are active:
        <ul .users>
            $forall Entity uid u<-users
                <li>
                    <a href=@{HomeR uid}>#{userName u}
                        <span .note>registered on #{formatDateStr (userInserted u)}
        |]
        toWidget [lucius|
        .note{
            margin-left: 1em;
            float: right;
        }
        ul.users{
            list-style: none;
            padding: 0;
            border: 1px solid #dce4ec;
        }
        ul.users>li>a{
            padding: 0.75em 2em;
            display: block;
        }
        ul.users>li:nth-child(odd){
            background-color: white;
        }
            
        |]

formatDateStr :: UTCTime -> String
formatDateStr t = formatTime defaultTimeLocale "%e %b %Y" t