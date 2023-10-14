{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Page0 where

import Import

getPage0R :: Text -> Handler Html
getPage0R text = do
    --appAdministratorId <- requireAppAdministratorId
    mEntry<- runDB $ selectFirst [EntryInputTitle==.text,EntryType==.Page0,EntryStatus==.Publish] [Desc EntryInserted]
    case mEntry of
        Nothing -> notFound
        Just (Entity _ entry) -> defaultLayout $ do 
                setTitle $ toHtml text
                [whamlet|
<article .entry :entryStatus entry == Draft:.draft>
    <h1>#{text}
    <div .entry-content>
      <div .entry-content-wrapper>#{preEscapedToMarkup (entryOutputBody entry)}
    <!--  <ul .entry-menu>
        <li>
            <a href=@{EditPage0R text}>_{MsgEdit}-->
                |]

