{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.UserPage where

import Import
import Parse.Parser(scaleHeader)
import Data.Text(strip)

getUserPageR :: UserId -> Text -> Handler Html
getUserPageR userId _ = do
    author<-runDB $ get404 userId
    mCurrentUserId<-maybeAuthId
    -- Currently, support about page only
    mEntry<-runDB $ selectFirst [EntryType==.UserPage,EntryUserId==.userId,EntryStatus==.Publish] [Desc EntryInserted]

    case mEntry of
        Nothing->
            defaultLayout $ do
                setTitleI MsgAbout
                [whamlet|
<article .entry>
    <h1>_{MsgAbout}
    <div .entry-content>
        <div .entry-content-wrapper>
            <p>
                <img src=@{StaticR $ StaticRoute ["icons","user-photo.png"] []} title="Avatar" class="float-right" style="height:6em;">
            <h3 id="basic-information">Basic Information
            <ul>
                <li><strong>Name</strong>: #{userName author}
                <li><strong>Homepage</strong>: 
                    <a href=@{UserHomeR userId}>@{UserHomeR userId}
    $if mCurrentUserId == Just userId
        <div.menu>
            <ul.list-inline.text-lowercase>
                <li>
                    <a.text-muted href=@{EditUserPageR "About"}>_{MsgEdit}
                    |]
                
        Just (Entity _ entry) ->
            defaultLayout $ do 
                setTitleI MsgAbout
                [whamlet|
<article .entry :entryStatus entry == Draft:.draft>
  <h1 .entry-title>#{preEscapedToMarkup(scaleHeader 1 (entryTitleHtml entry))}

  <div .entry-content>
      <div .entry-content-wrapper>
        $if strip (entryBodyHtml entry) /= "" 
            #{preEscapedToMarkup(entryBodyHtml entry)}
        $else
            _{MsgComingSoon}

  $if mCurrentUserId == Just userId
    <div .menu>
        <ul.list-inline.text-lowercase>
            <li>
                <a.text-muted href=@{EditUserPageR "About"}>_{MsgEdit}
                |]


