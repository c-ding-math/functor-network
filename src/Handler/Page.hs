{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Page where

import Import
import Handler.Parse

getPageR :: Text -> Handler Html
getPageR text = do
    --appAdministratorId <- requireAppAdministratorId
    mEntry<- runDB $ selectFirst [EntryTitle==.text,EntryType==.Page,EntryStatus==.Publish] [Desc EntryInserted]
    case mEntry of
        Nothing -> notFound
        Just (Entity entryId entry) -> do
            bodyHtml <- entryBodyHtmlCache entryId
            defaultLayout $ do 
                setTitle $ toHtml text
                [whamlet|
<article .entry :entryStatus entry == Draft:.draft>
    <h1>#{text}
    <div .entry-body>
      <div .entry-content-wrapper>#{preEscapedToMarkup bodyHtml}
    <!--  <ul .entry-menu>
        <li>
            <a href=@{EditPageR text}>_{MsgEdit}-->
                |]

