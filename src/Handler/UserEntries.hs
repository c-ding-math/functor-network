{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.UserEntries where

import Import
import Yesod.Form.Bootstrap3
import Handler.NewUserSubscription (subscribeToUserWidget)
import Data.Text (toLower)
import Handler.Entries (entryListWidget)

searchForm :: Form Text
searchForm = renderBootstrap3 BootstrapBasicForm $ areq searchFieldWithAddon searchFieldSettings Nothing where
    
    aSearchFiled = searchField False
    searchFieldWithAddon = aSearchFiled {
        fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
            [whamlet|
                <div .input-group>
                    <input .form-control type=search id=#{idAttr} name=#{nameAttr} *{otherAttrs} :isReq:required :not isReq:autofocus>
                    <span .input-group-btn>
                        <button .btn .btn-default type=submit>
                            _{MsgSearch}
            |]
    }
    searchFieldSettings = FieldSettings {
        fsLabel = "", 
        fsTooltip = Nothing,
        fsId = Nothing,
        fsName = Just "keyword",
        fsAttrs = [("class","form-control"),("placeholder","keyword"),("style","min-width:6em;")]
    }

getUserEntriesR :: UserId -> Handler Html
getUserEntriesR authorId = do
    mCurrentUserId<-maybeAuthId
    ((searchFormResult, searchFormWidget), searchFormEnctype) <- runFormGet searchForm

    (userEntryList,entryList,author,mSeachText)<-runDB $ do
        author<-get404 authorId      
        entries<- selectList [EntryUserId==.authorId, EntryType==.UserPost] [Desc EntryInserted]
        
        let userEntryList = [x | x<-entries, entryStatus (entityVal x) == Publish||isAdministrator mCurrentUserId (entityVal x)]          
        (entryList, mSeachText)<- case searchFormResult of
                FormSuccess searchText -> do
                    --setMessage $ toHtml $ "Search results for: " <> searchText
                    let 
                        searchTexts = Data.Text.toLower <$> words searchText
                        searchArea :: Entity Entry -> Text
                        searchArea x = Data.Text.toLower $ case entryBody (entityVal x) of
                            Nothing -> entryTitle (entityVal x)
                            Just body -> entryTitle (entityVal x) <> unTextarea body
                        
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
                        <a .btn.btn-primary .new-entry href=@{NewUserEntryR}>_{MsgNewPost}
            
            $maybe searchText <- mSeachText
                <p>_{MsgPostsContaining searchText}:
            $if null entryList
                
                $if mCurrentUserId == Just authorId
                    $if null userEntryList
                        <p>_{MsgNoPost} #
                            <a href=@{NewUserEntryR}>_{MsgFirstPost}
                        
                $else
                    <p>_{MsgNoPost}
            $else
                ^{entryListWidget "no-author" entryList}
        |]
        toWidget [hamlet|
            <div style="display:none;"><a href=@{CommentsR authorId}>Comments</a></div>
        |]
        toWidget [lucius| 
            .page-header {
                display: flex;
                justify-content: space-between;
                align-items: center;
            }
            .page-header-menu {
                display: flex;
                justify-content: space-between;
            }
            .page-header-menu > * {
                margin: 0.25em;
            }

            .search-form{
                flex-grow:1;
                margin:0 1em;
            }
            .search-form .form-group{
                margin-bottom:0;
            }
        |]
        


