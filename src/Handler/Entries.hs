{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Entries where

import Import
import Parse.Parser (scaleHeader)
import Yesod.Form.Bootstrap3
import Handler.NewUserSubscription (subscribeToUserWidget)
import Data.Text (toLower)
import Handler.Entry (formatDateStr)

searchForm :: Form Text
searchForm = renderBootstrap3 BootstrapBasicForm $ areq (searchField False) searchFieldSettings Nothing where
    searchFieldSettings = FieldSettings {
        fsLabel = "", 
        fsTooltip = Nothing,
        fsId = Nothing,
        fsName = Just "keyword",
        fsAttrs = [("class","form-control"),("placeholder","Search")]
    }

getEntriesR :: UserId -> Handler Html
getEntriesR authorId = do
    mCurrentUserId<-maybeAuthId
    ((searchFormResult, searchFormWidget), searchFormEnctype) <- runFormGet searchForm

    (userEntryList,entryList,author,mSeachText)<-runDB $ do
        author<-get404 authorId      
        entries<- selectList [EntryUserId==.Just authorId, EntryType==.Standard] [Desc EntryInserted]
        
        let userEntryList = [x | x<-entries, entryStatus (entityVal x) == Publish||isAdministrator mCurrentUserId (entityVal x)]          
        (entryList, mSeachText)<- case searchFormResult of
                FormSuccess searchText -> do
                    --setMessage $ toHtml $ "Search results for: " <> searchText
                    let 
                        searchTexts = Data.Text.toLower <$> words searchText
                        searchArea :: Entity Entry -> Text
                        searchArea x = Data.Text.toLower $ entryInputTitle (entityVal x) <> unTextarea (entryInputBody (entityVal x))
                        
                    return ([x | x<-userEntryList, all (\keyword -> keyword `isInfixOf` (searchArea x)) searchTexts], Just searchText)
                _ -> return (userEntryList, Nothing)

        return $ (userEntryList,entryList,author,mSeachText)
    
    defaultLayout $ do
        setTitle $ toHtml $ userName author
        [whamlet|
            <div .page-header>       
                <h1>_{MsgPosts}
                <form .search-form method=get enctype=#{searchFormEnctype}>
                    ^{searchFormWidget}
                <div .page-header-menu>
                    
                    ^{subscribeToUserWidget authorId}
                    $if mCurrentUserId == Just authorId
                        <a .btn.btn-primary .new-entry href=@{NewEntryR}>_{MsgNewPost}
            
            $maybe searchText <- mSeachText
                <p>_{MsgPostsContaining searchText}:
            $if null entryList
                
                $if mCurrentUserId == Just authorId
                    $if null userEntryList
                        <p>_{MsgNoPost} #
                            <a href=@{NewEntryR}>_{MsgFirstPost}
                        <p>Note: Accounts registered but never used may be considered as spam and deleted. To avoide your account being cleaned up by mistake, please post something.
                $else
                    <p>_{MsgNoPost}
            $else
                ^{entryListWidget entryList}
        |]
        toWidget [hamlet|
            <div style="display:none;"><a href=@{CommentsR authorId}>Comments</a></div>
        |]
        toWidget [lucius| 
            .search-form{
                flex-grow:1;
                margin:0 1em;
            }
            .search-form .form-group{
                margin-bottom:0;
            }
        |]
        
entryListWidget :: [Entity Entry] -> Widget
entryListWidget entryList = do
    toWidget [hamlet|
    <div .entries>
        <ul>
            $forall Entity entryId entry<-entryList
                $maybe authorId <- entryUserId entry
                    <li :entryStatus entry == Draft:.draft data-date=#{formatDateStr (entryInserted entry)}>
                        <a href=@{EntryR authorId entryId}>
                            <h3 .entry-title>#{preEscapedToMarkup (scaleHeader 3 (entryOutputTitle entry))}
                        
                            
    |]
    toWidget [lucius|
.entries>ul{   
    list-style:none;
    padding-left:0;
}
.entries>ul>li::before {
    content: attr(data-date);
    color: #b4bcc2;
}
.entries>ul>li>a>h3{
    color:black;
    margin:0.2em 0 1em;
}

    |]

