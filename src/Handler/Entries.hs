{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Entries where

import Import
import Parse.Parser (scaleHeader)

getEntriesR :: UserId -> Handler Html
getEntriesR authorId = do
    mCurrentUserId<-maybeAuthId
    (entryList,author)<-runDB $ do
        author<-get404 authorId      
        entries<- selectList [EntryUserId==.Just authorId, EntryType==.Standard] [Desc EntryInserted]
        let entryList =[x | x<-entries, entryStatus (entityVal x) == Publish||isAdministrator mCurrentUserId (entityVal x)]          
        return $ (entryList,author)
    
    defaultLayout $ do
        setTitle $ toHtml $ userName author
        [whamlet|        
            <h1>_{MsgPosts}
                $if mCurrentUserId == Just authorId
                    <a .btn.btn-primary .new-entry.navbar-right href=@{NewEntryR}>_{MsgNewPost}
            $if null entryList
                <p>_{MsgNoPost}
                    $if mCurrentUserId == Just authorId                      
                        <a href=@{NewEntryR}>_{MsgFirstPost}
            $else
                ^{entryListWidget entryList}
        |]
        toWidget [hamlet|
            <div style="display:none;"><a href=@{CommentsR authorId}>Comments</a></div>
        |]

entryListWidget :: [Entity Entry] -> Widget
entryListWidget entryList = do
    toWidget [hamlet|
    <div .entries>
        <ul>
            $forall Entity entryId entry<-entryList
                $maybe authorId <- entryUserId entry
                    <li :entryStatus entry == Draft:.draft>
                        <a href=@{EntryR authorId entryId}>
                            <h2>#{preEscapedToMarkup (scaleHeader 2 (entryOutputTitle entry))}
                        <div .tags>
                            <ul>
                                $forall (inputTag, outputTag) <- zip (entryInputTags entry) (entryOutputTags entry)
                                    <li>
                                        <a href=@{TagR inputTag}>#{preEscapedToMarkup outputTag}
    |]
    toWidget [lucius|
.entries>ul, .tags>ul {
    list-style:none;
    padding-left:0;
}

.entries>ul>li{
    border-top:1px solid #dce4ec;
    padding-bottom:1em;
}
.entries>ul>li>a>h2, .entries>ul>li>a>h3, .tags ul>li>a {
    color:black;
}

.tags ul>li{
    display:inline-block;
    padding:0.5em;
    background-color:whitesmoke;
    margin-bottom:1em;
}
li h1{
    color:black;
    font-size:2em;
}
    |]

