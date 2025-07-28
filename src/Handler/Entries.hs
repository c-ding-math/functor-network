
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Entries where

import Import
import Parse.Parser (scaleHeader)
import Yesod.Form.Bootstrap3
import Data.Text (toLower)

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

getEntriesR :: Handler Html
getEntriesR = do
    entryList'<-runDB $ selectList [EntryType==.UserPost,EntryStatus==.Publish, EntryFeatured==.True] [Desc EntryInserted, LimitTo 1000]
    --let entryList = [x | x<-entries, not (any (`isInfixOf` (entryBodyHtml (entityVal x))) ["lost proof","parser-message","filter-information"])]   
    ((searchFormResult, searchFormWidget), searchFormEnctype) <- runFormGet searchForm
    (entryList, mSeachText)<- case searchFormResult of
        FormSuccess searchText -> do
            --setMessage $ toHtml $ "Search results for: " <> searchText
            let 
                searchTexts = Data.Text.toLower <$> words searchText
                searchArea :: Entity Entry -> Text
                searchArea x = Data.Text.toLower $ case entryBody (entityVal x) of
                    Nothing -> entryTitle (entityVal x)
                    Just body -> entryTitle (entityVal x) <> unTextarea body
                
            return ([x | x<-entryList', all (\keyword -> keyword `isInfixOf` (searchArea x)) searchTexts], Just searchText)
        _ -> return (entryList', Nothing)

    defaultLayout $ do
        setTitleI MsgPosts
        setDescriptionIdemp $ "Latest posts on " <> appName
        [whamlet|
            <div .page-header style="min-width:520px;">  
                <form .search-form .pull-right method=get enctype=#{searchFormEnctype} style="max-width:15em;">
                    ^{searchFormWidget}     
                <h1 style="white-space: nowrap">_{MsgLatestPosts}

                
            $maybe searchText <- mSeachText
                <p>_{MsgPostsContaining searchText}:        
            $if null entryList
                <p>_{MsgNoPost} #

            $else
                ^{entryListWidget "" entryList}
     
        |]


entryListWidget :: Text ->  [Entity Entry] -> Widget
entryListWidget style entryList = do
    listData <- handlerToWidget $ do
        (authors, categoryLists)<-runDB $ do
            authors <- mapM (get404 . entryUserId . entityVal) entryList
            categoryLists <- forM entryList $ \entry -> do
                let entryId = entityKey entry
                entryCategoryIds<- map (entryTreeParent . entityVal) <$> selectList [EntryTreeNode ==. entryId] []
                selectList [EntryId <-. entryCategoryIds] [Desc EntryInserted]
            return (authors, categoryLists)
        
        return $ zip3 entryList authors categoryLists

    [whamlet|
<ul .list-group .entry-list .#{style}>
    $forall (Entity entryId entry, author, entryCategoryEntities)<- listData
        <li .list-group-item .entry-item :entryStatus entry == Draft:.draft>
            <div .entry-meta>
                <ul.list-inline>
                    <li .by>
                        <span title="author"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-person" viewBox="0 0 16 16"><path d="M8 8a3 3 0 1 0 0-6 3 3 0 0 0 0 6m2-3a2 2 0 1 1-4 0 2 2 0 0 1 4 0m4 8c0 1-1 1-1 1H3s-1 0-1-1 1-4 6-4 6 3 6 4m-1-.004c-.001-.246-.154-.986-.832-1.664C11.516 10.68 10.289 10 8 10s-3.516.68-4.168 1.332c-.678.678-.83 1.418-.832 1.664z"/></svg>
                        <a href=@{UserHomeR (entryUserId entry)}>#{userName author}
                    <li .at>
                        <span title="date"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-clock" viewBox="0 0 16 16"><path d="M8 3.5a.5.5 0 0 0-1 0V9a.5.5 0 0 0 .252.434l3.5 2a.5.5 0 0 0 .496-.868L8 8.71z"/><path d="M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16m7-8A7 7 0 1 1 1 8a7 7 0 0 1 14 0"/></svg>
                        $if entryInserted entry == entryUpdated entry
                            #{utcToDate (entryInserted entry)}
                        $else
                            <span .dropdown>
                                <a .text-muted.dropdown-toggle data-toggle=dropdown>
                                    #{utcToDate (entryInserted entry)}
                                    <span .caret>
                                <ul .dropdown-menu>
                                    <li>
                                        <a .text-muted onclick="return false;" href=#>
                                            created #{utcToDateTime (entryInserted entry)}
                                    <li>
                                        <a .text-muted onclick="return false;" href=#>
                                            updated #{utcToDateTime (entryUpdated entry)}
                    $if not (null entryCategoryEntities)
                        <li .in>
                            <span title="category"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-folder" viewBox="0 0 16 16"><path d="M.54 3.87.5 3a2 2 0 0 1 2-2h3.672a2 2 0 0 1 1.414.586l.828.828A2 2 0 0 0 9.828 3h3.982a2 2 0 0 1 1.992 2.181l-.637 7A2 2 0 0 1 13.174 14H2.826a2 2 0 0 1-1.991-1.819l-.637-7a2 2 0 0 1 .342-1.31zM2.19 4a1 1 0 0 0-.996 1.09l.637 7a1 1 0 0 0 .995.91h10.348a1 1 0 0 0 .995-.91l.637-7A1 1 0 0 0 13.81 4zm4.69-1.707A1 1 0 0 0 6.172 2H2.5a1 1 0 0 0-1 .981l.006.139q.323-.119.684-.12h5.396z"/></svg>
                            <span .dropdown>
                                <a .text-muted.dropdown-toggle data-toggle=dropdown>
                                    #{show $ length entryCategoryEntities} #
                                    $if length entryCategoryEntities <= 1
                                        category 
                                    $else
                                        categories 
                                    <span .caret>
                                <ul .dropdown-menu>
                                    $forall (Entity categoryId category)<-entryCategoryEntities
                                        <li>
                                            <a href=@{CategoriesR (entryUserId category)}#{"#entry-" <> toPathPiece categoryId}>#{preEscapedToMarkup $ entryTitleHtml category}

            <a.stretched-link href=@{UserEntryR (entryUserId entry) entryId}>
                <h3 .entry-title>#{preEscapedToMarkup(scaleHeader 3 (entryTitleHtml entry))}
    |]
