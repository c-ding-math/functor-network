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
<h1>#{text}
#{preEscapedToMarkup (entryOutputBody entry)}
<!--  <ul .entry-menu>
      <li>
          <a href=@{EditPage0R text}>_{MsgEdit}-->
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
