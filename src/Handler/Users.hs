{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Users where

import Import

getUsersR :: Handler Html
getUsersR = do
    
    users <- runDB $ do 
        users'<- selectList [] [Asc UserInserted]
        filterM (\(Entity uid _) -> do
            entries <- selectList [EntryUserId==.Just uid] [Desc EntryInserted]
            return $ not (Prelude.null entries)) users'
    
    defaultLayout $ do
        setTitleI MsgUsers
        [whamlet|
<h1>_{MsgActiveUsers}
$if Prelude.null users
    <div>_{MsgNothingFound}
$else 
    <div .entries>
        <ul .users>
            $forall Entity uid u<-users
                <li>
                    <a href=@{HomeR uid}>#{userName u}
                    <span .note>registered on #{formatDateStr (userInserted u)}
        |]
        toWidget [lucius|
        .note{
            margin-left: 1em;
        }
        ul.users>li{
            margin-bottom: 0.5em;
        }
        |]

formatDateStr :: UTCTime -> String
formatDateStr t = formatTime defaultTimeLocale "%e %b %Y" t