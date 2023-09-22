
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Entries0 where

import Import
import Handler.Entries (entryListWidget)
getEntries0R :: Handler Html
getEntries0R = do
    entryList<-runDB $ selectList [EntryType==.Standard,EntryStatus==.Publish] [Desc EntryInserted]
    defaultLayout $ do
        setTitleI MsgPosts
        [whamlet|
            <div .page-header>       
                <h1>_{MsgPosts}
                <div .page-header-menu>
                    
            $if null entryList
                <p>_{MsgNoPost} #

            $else
                ^{entryListWidget entryList}
        |]
