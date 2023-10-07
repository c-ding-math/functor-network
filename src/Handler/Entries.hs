{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Entries where

import Import
import Parse.Parser (scaleHeader)
import Yesod.Form.Bootstrap3
import Handler.NewUserSubscription (subscribeToUserWidget)

{-searchForm :: Form Text
searchForm = renderBootstrap3 BootstrapBasicForm $ areq (searchField False) (bfs ("Search"::Text)) Nothing
-}
getEntriesR :: UserId -> Handler Html
getEntriesR authorId = do
    mCurrentUserId<-maybeAuthId
    --(searchFormWidget, searchFormEnctype) <- generateFormPost searchForm

    (entryList,author)<-runDB $ do
        author<-get404 authorId      
        entries<- selectList [EntryUserId==.Just authorId, EntryType==.Standard] [Desc EntryInserted]
        --searchParam <- lookupGetParam "q"
        let entryList = [x | x<-entries, entryStatus (entityVal x) == Publish||isAdministrator mCurrentUserId (entityVal x)]          
            {-entryList = case searchParam of
                Just searchText -> [x | x<-entryList', searchText `isInfixOf` (entryInputTitle (entityVal x) <> unTextarea (entryInputBody (entityVal x)))]
                _ -> entryList'-}

        return $ (entryList,author)
    
    defaultLayout $ do
        setTitle $ toHtml $ userName author
        [whamlet|
            <div .page-header>       
                <h1>_{MsgPosts}
                <div .page-header-menu>
                    
                    ^{subscribeToUserWidget authorId}
                    $if mCurrentUserId == Just authorId
                        <a .btn.btn-primary .new-entry href=@{NewEntryR}>_{MsgNewPost}
            $if null entryList
                <p>_{MsgNoPost} #
                    $if mCurrentUserId == Just authorId                      
                        <a href=@{NewEntryR}>_{MsgFirstPost}
                $if mCurrentUserId == Just authorId
                    <p>Note: Accounts registered but never used may be considered as spam and deleted. To avoide your account being cleaned up by mistake, please post something.

            $else
                <!--<form method=get enctype=#{searchFormEnctype}>
                    ^{searchFormWidget}
                    <button type="submit" .btn.btn-primary>_{MsgSearch}-->
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
                            <h2 .entry-title>#{preEscapedToMarkup (scaleHeader 2 (entryOutputTitle entry))}
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
    padding:0.25em 0.5em;
    background-color:#eee;
    margin-bottom:1em;
}
li h1{
    color:black;
    font-size:2em;
}
    |]

