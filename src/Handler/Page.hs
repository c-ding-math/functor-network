{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Page where

import Import

getPageR :: UserId -> Text -> Handler Html
getPageR userId _ = do
    author<-runDB $ get404 userId
    mCurrentUserId<-maybeAuthId
    -- Currently, support about page only
    mEntry<-runDB $ selectFirst [EntryInputTitle==."About",EntryType==.Page,EntryUserId==.userId,EntryStatus==.Publish] [Desc EntryInserted]

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
                    <a href=@{HomeR userId}>@{HomeR userId}
    $if mCurrentUserId == Just userId
        <ul .entry-menu>
            <li>
                <a href=@{EditPageR "About"}>_{MsgEdit}
                |]
                
        Just (Entity _ entry) ->
            defaultLayout $ do 
                setTitleI MsgAbout
                [whamlet|
<article .entry :entryStatus entry == Draft:.draft>
    <h1>_{MsgAbout}
    <div .entry-content>
        <div .entry-content-wrapper>#{preEscapedToMarkup (entryOutputBody entry)}
    $if mCurrentUserId == Just userId
        <ul .entry-menu>
            <li>
                <a href=@{EditPageR "About"}>_{MsgEdit}
                |]


