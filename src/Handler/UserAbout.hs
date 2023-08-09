{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.UserAbout where

import Import

getUserAboutR :: UserId -> Handler Html
getUserAboutR userId = do
    _<-runDB $ get404 userId
    mCurrentUserId<-maybeAuthId
    mEntry<-runDB $ selectFirst [EntryInputTitle==."About",EntryType==.Page,EntryUserId==.userId] [Desc EntryInserted]

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
                menuWidget
                
        Just (Entity _ entry) ->
            defaultLayout $ do 
                setTitleI MsgAbout
                [whamlet|
<h1>_{MsgAbout}
#{preEscapedToMarkup (entryOutputBody entry)}
<ul .entry-menu>
    <li>
        <a href=@{EditPageR "About"}>_{MsgEdit}
                |]
                menuWidget
                
menuWidget:: Widget
menuWidget= toWidget [lucius|
.entry-menu,.comment-menu{
  list-style-type: none;
  padding-left:0;
  text-transform:lowercase;
}
.entry-menu>li,.comment-menu>li{
  display: inline-block;
  margin-right:1em;
  margin-bottom:2em;
}
.entry-menu a,.comment-menu a{
    color:#b4bcc2;
}
                |]

    

