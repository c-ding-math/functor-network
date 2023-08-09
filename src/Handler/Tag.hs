{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Tag where

import Import
import Handler.Parser (tagsWidget)

getTagR :: Path -> Text -> Handler Html
getTagR piece tag = do
    mCurrentUserId<-maybeAuthId
    entryList<-runDB $ do
        entries <- selectList [EntryUserId==.piece] [Desc EntryInserted]
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
                                <a href=@{EntryR piece entryId}>
                                    <h2>#{preEscapedToMarkup (entryOutputTitle entry)}
                                <div .tags>
                                    ^{tagsWidget piece (zip (entryInputTags entry) (entryOutputTags entry))}
        |]
        $(widgetFile "entry-list")

