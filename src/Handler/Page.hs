{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Page where

import Import

getPageR :: UserId -> Text -> Handler Html
getPageR userId _ = do
    _<-runDB $ get404 userId
    mCurrentUserId<-maybeAuthId
    -- Currently, support about page only
    mEntry<-runDB $ selectFirst [EntryInputTitle==."About",EntryType==.Page,EntryUserId==.userId,EntryStatus==.Publish] [Desc EntryInserted]

    case mEntry of
        Nothing->
            defaultLayout $ do
                setTitleI MsgAbout
                [whamlet|
<h1>_{MsgAbout}
<p>Apparently, this user prefers to keep an air of mystery.
$if mCurrentUserId == Just userId
    <ul .entry-menu>
        <li>
            <a href=@{EditPageR "About"}>_{MsgEdit}
                |]
                
        Just (Entity _ entry) ->
            defaultLayout $ do 
                setTitleI MsgAbout
                [whamlet|
<h1>_{MsgAbout}
#{preEscapedToMarkup (entryOutputBody entry)}
$if mCurrentUserId == Just userId
    <ul .entry-menu>
        <li>
            <a href=@{EditPageR "About"}>_{MsgEdit}
                |]


