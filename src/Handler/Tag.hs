{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Tag where

import Import
import Handler.Parser (tagsWidget)
import Parse.Parser (scaleHeader)

getTagR :: Text -> Handler Html
getTagR tag = do
    mCurrentUserId<-maybeAuthId
    entryList<-runDB $ do
        entries <- selectList [EntryType==.Standard] [Desc EntryInserted]
        return $ [x | x<-entries, tag `elem` (entryInputTags . entityVal) x, entryStatus (entityVal x) == Publish||isAdministrator mCurrentUserId (entityVal x)]
    defaultLayout $ do
        setTitleI MsgTag
        [whamlet|
            <h1>_{MsgTag}: #{tag}
            $if null entryList
                <div> _{MsgNothingFound}
            $else
                <div .entries>
                    <ul>
                        $forall Entity entryId entry<-entryList
                            <li :entryStatus entry == Draft:.draft>
                                <a href=@{EntryR (entryUserId entry) entryId}>
                                    <h2>#{preEscapedToMarkup (scaleHeader 2 (entryOutputTitle entry))}
                                <div .tags>
                                    ^{tagsWidget (zip (entryInputTags entry) (entryOutputTags entry))}
        |]
        addStylesheet $ StaticR css_entry_list_css

