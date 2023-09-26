{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Users where

import Import

getUsersR :: Handler Html
getUsersR = do
    
    users <- runDB $ selectList [] [Asc UserInserted]

    defaultLayout $ do
        setTitleI MsgUsers
        [whamlet|
<h1>_{MsgUsers}
$if Prelude.null users
    <div>_{MsgNothingFound}
$else 
    <div .entries>
        <ul .users>
            $forall Entity uid u<-users
                <li>
                    <a href=@{PageR uid "About"}>#{userName u}
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