{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Tree where

import Import
import Data.List (intersect)
import Yesod.Form.Bootstrap3
import Text.Shakespeare.Text (stext)

data Parents = Parents
    { _ids :: Maybe [EntryId]
    }

treeForm :: UserId -> Maybe Parents -> Form (Parents)
treeForm userId mSelectedList = renderBootstrap3 BootstrapBasicForm $ Parents
        <$> aopt (multiSelectField options) (bfs MsgSelectCategories) (_ids <$> mSelectedList)
    where
        options = optionsPersistKey [EntryUserId ==. userId, EntryType ==. Category] [Desc EntryInserted] entryTitle

postTreeR :: EntryId -> Handler Html
postTreeR node = do 
    userId <- requireAuthId
    entry <- runDB $ get404 node
    ((result, _), _) <- runFormPost $ treeForm userId Nothing
    case result of
        FormSuccess parents -> do
                --deleteWhere [EntryTreeNode ==. node]
                case _ids parents of
                    Just parentIds -> do
                        forM_ parentIds $ \parentId -> do
                            parent <- runDB $ get404 parentId
                            when (entryType parent == Category) $ do
                                eTree <- runDB $ insertBy $ EntryTree node parentId
                                case eTree of
                                    Left _ -> return ()
                                    Right _ -> do
                                        urlRenderParams <- getUrlRenderParams
                                        subscribers <- runDB $ selectList [EntrySubscriptionEntryId ==. parentId, EntrySubscriptionVerified ==. True] []
                                        forM_ subscribers $ \(Entity subscriptionId subscription) -> do
                                            
                                            let unsubscribeUrl= urlRenderParams (EditEntrySubscriptionR subscriptionId) $ case entrySubscriptionKey subscription of
                                                    Just key -> [("key", key)] 
                                                    Nothing -> []
                                                entryUrl= urlRenderParams (CategoriesR (entryUserId parent)) [] <> "#entry-" <> toPathPiece parentId
                                            sendAppEmail (entrySubscriptionEmail subscription) $ categorySubscriptionNotification unsubscribeUrl entryUrl (entryTitle entry) (entryTitle parent)                        

                        setMessageI $ MsgPostCategorized
                    Nothing -> return ()
            
        FormMissing -> do
            setMessageI MsgFormMissing
        FormFailure _ -> do
            setMessageI MsgFormFailure
    redirect $ UserEntryR (entryUserId entry) node

treeWidget :: EntryId -> Widget
treeWidget entryId = do
    maybeUserId <- maybeAuthId
    case maybeUserId of
        Just userId -> do
            (widget, enctype, categories) <- handlerToWidget $ do
                (parentIds, categories) <- runDB $ do
                    categories <- selectList [EntryUserId ==. userId, EntryType ==. Category] [Desc EntryInserted]
                    entryTreeEntities <- selectList [EntryTreeNode ==. entryId] []
                    return (Data.List.intersect (map entityKey categories) (map (entryTreeParent . entityVal) entryTreeEntities), categories)
                (widget, enctype) <- generateFormPost $ treeForm userId $ Just $ Parents $ Just parentIds
                return (widget, enctype, categories)
            [whamlet|
        <div .modal.fade>
            <div .modal-dialog>
                <div .modal-content>
                    <div .modal-header>
                        <button type=button .close data-dismiss=modal>&times;
                        <b .modal-title>_{MsgCategorize}
                    <div .modal-body>
                        $if null categories
                            <div #category-form>
                                <p>There are no categories available for selection. You can go to the <a target=_blabk href=@{CategoriesR userId}>categories page</a> to create a category first.
                                <div .text-right>
                                    <a .btn.btn-primary target=_blabk href=@{CategoriesR userId}>_{MsgCreateACategory}
                        $else    
                            <div .list-group>
                                <label>_{MsgSelectCategories}
                                $forall Entity categoryId category <- categories
                                    <a.list-group-item href=# data-id=#{toPathPiece categoryId}>#{preEscapedToMarkup $ entryTitleHtml category}

                            <form .hidden #category-form method=post action="" enctype=#{enctype}>
                                ^{widget}
                            <div .text-right>
                                <button form=category-form .btn.btn-primary type=submit>_{MsgCategorize}
                                
            |]
            toWidget [julius|
                $(document).ready(function(){
                    var categoryForm = $("#category-form");
                    var categoryListGroup = categoryForm.siblings(".list-group");
                    $(".categorize a").click(function(){
                        
                        categoryForm.attr("action", $(this).data("action"));
                        //if option is selected, set corresponding list item to active
                        var selected = categoryForm.find("option:selected");
                        if (selected.length > 0) {
                            categoryForm.find("a").removeClass("active");
                            selected.each(function(){
                                categoryListGroup.find("a[data-id=" + $(this).val() + "]").addClass("active");
                            });
                        }
                        categoryForm.closest(".modal").modal("show");
                        return false;
                    });
                    categoryListGroup.find("a").click(function(){
                        var id = $(this).data("id");
                        var option = categoryForm.find("option[value=" + id + "]");
                        if (option.length > 0) {
                            option.prop("selected", !option.prop("selected"));
                            $(this).toggleClass("active");
                        }
                        return false;
                    });
                });
            |]
        Nothing ->  toWidget [julius|
            $(".categorize  a").attr("href", "@{AuthR LoginR}");
                    |]

categorySubscriptionNotification :: Text -> Text -> Text -> Text -> AppEmail
categorySubscriptionNotification unsubscribeUrl entryUrl nodeTitle parentTitle = AppEmail emailSubject emailText emailHtml
    where
        emailSubject= "New post categorized in " <> parentTitle
        emailText = [stext|
There is a new post added to the category "#{parentTitle}":

#{nodeTitle}

View it at #{entryUrl}.

Manage your subscriptions at #{unsubscribeUrl}.
#{appName}
            |]
        emailHtml = [shamlet|
<p>There is a new post added to the category "#{parentTitle}":
<p>#{nodeTitle}
<p><a href=#{entryUrl}>View</a><span> | </span><a href=#{unsubscribeUrl}>Manage subscriptions</a>
<p>#{appName}
            |]


getRootEntryId :: EntryId -> ReaderT SqlBackend (HandlerFor App) EntryId
getRootEntryId entryId = do
    entry <- get404 entryId
    if entryType entry `elem` [UserPost, Post, UserPage, Page]
        then return entryId
        else do
            mEntryTree<-selectFirst [EntryTreeNode==.entryId] []
            case mEntryTree of
                Nothing -> return entryId
                Just tree -> getRootEntryId $ entryTreeParent $ entityVal tree
