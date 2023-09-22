{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Users where

import Import

getUsersR :: Handler Html
getUsersR = do
    
    users <- runDB $ selectList [] [Desc UserInserted]

    defaultLayout $ do
        setTitleI MsgUsers
        [whamlet|
<h1>_{MsgUsers}
$if Prelude.null users
    <div>_{MsgNothingFound}
$else 
    <div .entries>
        <ul>
            $forall Entity uid u<-users
                <li>
                    <a href=@{HomeR uid}>#{userName u}
                    <span>registered on #{formatDateStr (userInserted u)}
        |]

formatDateStr :: UTCTime -> String
formatDateStr t = formatTime defaultTimeLocale "%e %b %Y" t