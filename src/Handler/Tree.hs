{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Tree where

import Import
import Yesod.Form.Bootstrap3

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
            runDB $ do
                deleteWhere [EntryTreeNode ==. node]
                case _ids parents of
                    Just entryIds -> do
                        forM_ entryIds $ \entryId -> do
                            parent <- get404 entryId
                            when (entryType parent == Category) $ do
                                insert_ $ EntryTree node entryId
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
                    return ([x | x <- map entityKey categories , x <- map (entryTreeParent . entityVal) entryTreeEntities], categories)
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