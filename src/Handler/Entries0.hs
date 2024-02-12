
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Entries0 where

import Import
--import Handler.Entries (entryListWidget)
import Handler.Entry (formatDateStr)
import Parse.Parser (scaleHeader)

getEntries0R :: Handler Html
getEntries0R = do
    entryList<-runDB $ selectList [EntryType==.UserPost,EntryStatus==.Publish] [Desc EntryInserted]
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
                                                <a href=@{PageR (entryUserId entry) "About"}>#{userName $ author}
                                            $nothing 
                                                _{MsgUnregisteredUser}
                                        <span .at>#{formatDateStr (entryInserted entry)}
                                    <a href=@{EntryR (entryUserId entry) entryId}>
                                        <h3 .entry-title>#{preEscapedToMarkup(scaleHeader 3 (entryOutputTitle entry))}
                        
        |]
        toWidget [lucius|
            .entries ul>li>a>h3{
                color:black;
                margin:0 0 1em 0;
            }|]
