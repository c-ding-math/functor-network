{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Tag where

import Import
import Handler.Entries (entryListWidget)

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
                <p>_{MsgNoPost}
            $else
                ^{entryListWidget entryList}
        |]
        
