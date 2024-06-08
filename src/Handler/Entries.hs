
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Entries where

import Import
--import Handler.Entries (entryListWidget)
import Handler.UserEntry (formatDateStr)
import Parse.Parser (scaleHeader)


getEntriesR :: Handler Html
getEntriesR = do
    entries<-runDB $ selectList [EntryType==.UserPost,EntryStatus==.Publish] [Desc EntryInserted]
    let entryList = [x | x<-entries, not ("lost proof" `isInfixOf` (entryBodyHtml (entityVal x))), not ("parser-message" `isInfixOf` ((entryBodyHtml (entityVal x))<>(entryTitleHtml (entityVal x))))]
    mAuthors<-runDB $ do
        mapM (\x-> get $ entryUserId $ entityVal x) entryList 
        

    defaultLayout $ do
        setTitleI MsgPosts
        [whamlet|
            <div .page-header>       
                <h1>_{MsgLatestPosts}
                <div .page-header-menu>
                    
            $if null entryList
                <p>_{MsgNoPost} #

            $else
                <div .entries>
                    <ul>
                        $forall (Entity entryId entry, mAuthor)<- zip entryList mAuthors

                                <li :entryStatus entry == Draft:.draft>
                                    <div .entry-meta>
                                        <span .by>
                                            $maybe author<-mAuthor      
                                                <a href=#{userAbout author}>#{userName author}
                                            $nothing 
                                                _{MsgUnregisteredUser}
                                        <span .at>#{formatDateStr (entryInserted entry)}
                                    <a href=@{UserEntryR (entryUserId entry) entryId}>
                                        <h3 .entry-title>#{preEscapedToMarkup(scaleHeader 3 (entryTitleHtml entry))}
                        
        |]
        toWidget [lucius|
            .entries ul>li>a>h3{
                color:black;
                margin:0 0 1em 0;
            }|]
