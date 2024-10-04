
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Entries where

import Import
--import Handler.Entries (entryListWidget)
import Parse.Parser (scaleHeader)


getEntriesR :: Handler Html
getEntriesR = do
    entries<-runDB $ selectList [EntryType==.UserPost,EntryStatus==.Publish] [Desc EntryInserted]
    let entryList = [x | x<-entries, not (any (`isInfixOf` (entryBodyHtml (entityVal x))) ["lost proof","parser-message","filter-information"])]   
    defaultLayout $ do
        setTitleI MsgPosts
        [whamlet|
            <div .page-header>       
                <h1>_{MsgLatestPosts}
                <div .page-header-menu>
                    
            $if null entryList
                <p>_{MsgNoPost} #

            $else
                ^{entryListWidget "" entryList}
     
        |]


entryListWidget :: Text ->  [Entity Entry] -> Widget
entryListWidget style entryList = do
    listData <- handlerToWidget $ do
        authors<-runDB $ do
            mapM (\x-> get404 $ entryUserId $ entityVal x) entryList 
        return $ zip entryList authors

    [whamlet|
            <ul .entry-list .#{style}>
                $forall (Entity entryId entry, author)<- listData
                        <li :entryStatus entry == Draft:.draft>
                            <div .entry-meta>
                              <ul.list-inline>
                                <li .by>
                                    <span title="author"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-person" viewBox="0 0 16 16"><path d="M8 8a3 3 0 1 0 0-6 3 3 0 0 0 0 6m2-3a2 2 0 1 1-4 0 2 2 0 0 1 4 0m4 8c0 1-1 1-1 1H3s-1 0-1-1 1-4 6-4 6 3 6 4m-1-.004c-.001-.246-.154-.986-.832-1.664C11.516 10.68 10.289 10 8 10s-3.516.68-4.168 1.332c-.678.678-.83 1.418-.832 1.664z"/></svg>
                                    <a href=@{UserHomeR (entryUserId entry)}>#{userName author}
                                <li .at>
                                    <span title="date"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-clock" viewBox="0 0 16 16"><path d="M8 3.5a.5.5 0 0 0-1 0V9a.5.5 0 0 0 .252.434l3.5 2a.5.5 0 0 0 .496-.868L8 8.71z"/><path d="M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16m7-8A7 7 0 1 1 1 8a7 7 0 0 1 14 0"/></svg>
                                    #{utcToDate (entryInserted entry)}
                            <a href=@{UserEntryR (entryUserId entry) entryId}>
                                <h3 .entry-title>#{preEscapedToMarkup(scaleHeader 3 (entryTitleHtml entry))}
    |]
