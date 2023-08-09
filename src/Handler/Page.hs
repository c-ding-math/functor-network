{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Page where

import Import

getPageR :: Text -> Handler Html
getPageR text = do
    --appAdministratorId <- requireAppAdministratorId
    mEntry<-case text of
      "Features" ->  return Nothing
      "Screenshots" -> return Nothing
      _ -> runDB $ selectFirst [EntryInputTitle==.text,EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]
    case mEntry of
        Nothing -> notFound
        Just (Entity _ entry) -> defaultLayout $ do 
                setTitle $ toHtml text
                [whamlet|
<h1>#{text}
#{preEscapedToMarkup (entryOutputBody entry)}
<!--  <ul .entry-menu>
      <li>
          <a href=@{EditPageR text}>_{MsgEdit}-->
                |]
                toWidget [lucius|
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
