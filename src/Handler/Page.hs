{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Page where

import Import

getPageR :: Text -> Handler Html
getPageR text = do
    --appAdministratorId <- requireAppAdministratorId
    mEntry<- runDB $ selectFirst [EntryTitle==.text,EntryType==.Page,EntryStatus==.Publish] [Desc EntryInserted]
    case mEntry of
        Nothing -> notFound
        Just (Entity _ entry) -> defaultLayout $ do 
                setTitle $ toHtml text
                [whamlet|
<article .entry :entryStatus entry == Draft:.draft>
    <h1>#{text}
    <div .entry-content>
      <div .entry-content-wrapper>#{preEscapedToMarkup (entryBodyHtml entry)}
    <!--  <ul .entry-menu>
        <li>
            <a href=@{EditPageR text}>_{MsgEdit}-->
                |]

