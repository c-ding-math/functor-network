
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Categories where

import Import
import Handler.EditCategory (categoryForm, CategoryInput(..))
import Handler.Entries (entryListWidget)
import Parse.Parser (scaleHeader)
import Handler.NewEntrySubscription(subscribeToEntryWidget)


getCategoriesR :: UserId ->  Handler Html
getCategoriesR authorId = do 
    mCurrentUserId <- maybeAuthId
    let isAuthor = mCurrentUserId == Just authorId
    categoryAndEntryListList <- runDB $ do
        categoryList <- selectList [EntryUserId ==. authorId, EntryType ==. Category] [Desc EntryInserted]
        entryListList <- forM categoryList $ \category -> do
            entryTrees <- selectList [EntryTreeParent ==. entityKey category] []
            let entryIds = map (entryTreeNode . entityVal) entryTrees
            case mCurrentUserId of
                Just _ -> selectList [EntryId <-. entryIds] [Desc EntryInserted]
                    
                Nothing -> selectList [EntryId <-. entryIds, EntryStatus ==. Publish] [Desc EntryInserted]
        return $ zip categoryList entryListList
    (newCategoryWidget, enctype) <- generateFormPost $ categoryForm Nothing
    editCategoryFormList <- mapM (\(category, _) -> generateFormPost $ categoryForm $ Just $ CategoryInput $ entryTitle $ entityVal category) categoryAndEntryListList
    
    defaultLayout $ do
        setTitleI MsgCategories 
        [whamlet|
            <div .page-header>       
                $if isAuthor
                    <div .pull-right>         
                        <a .btn.btn-default .new-entry href=#>_{MsgNewCategory}
                <h1>_{MsgCategories}
            $if null categoryAndEntryListList
                $if isAuthor
                    <ul .category-list>
                        <li style="display:none;" .new-item>
                            <div .category>
                                <form .category-form .form-inline method=post action=@{NewCategoryR} enctype=#{enctype}>
                                    ^{newCategoryWidget}
                                    <button .btn.btn-primary type=submit>_{MsgCreateCategory}
                                    <a .btn.btn-default.cancel href=#>_{MsgCancel}
                    <p>_{MsgNoCategory} #
                        <a .new-entry href=#>_{MsgCreateACategory}        
                $else
                    <p>_{MsgNoCategory}
            $else
                <ul .category-list>
                    $if isAuthor
                        <li style="list-style:none;margin-bottom:1em;">
                            <form .form-inline>
                                <div .form-group>
                                    <label.text-lowercase for=filter>_{MsgPostFilter}
                                    <select #filter .form-control>
                                        <option value="all">_{MsgAllPosts}
                                        <option value="own">_{MsgMyOwnPostsOnly}
                                        <option value="others">_{MsgOthersPosts}
                        <li style="display:none;" .new-item>
                            <div .category>
                                <form .category-form .form-inline method=post action=@{NewCategoryR} enctype=#{enctype}>
                                    ^{newCategoryWidget}
                                    <button .btn.btn-primary type=submit>_{MsgCreateCategory}
                                    <a .btn.btn-default.cancel href=#>_{MsgCancel}
                    $forall ((category, entryList), (formWidget, enctype)) <- zip categoryAndEntryListList editCategoryFormList
                        <li>
                            <div .category #entry-#{toPathPiece (entityKey category)}> 
                                <h4 .entry-title style="display:inline;">#{preEscapedToMarkup $ scaleHeader 4 $ entryTitleHtml $ entityVal category}
                                <span style="display:inline-block;margin-left:2em;">
                                    <ul.list-inline.text-lowercase>
                                        <li .subscribe>
                                            <a.text-muted href=# data-action=@{NewEntrySubscriptionR $ entityKey category}>_{MsgFollow}
                                        $if isAuthor
                                            <li .edit-entry>
                                                <a .text-muted href=@{EditCategoryR $ entityKey category}>_{MsgEdit}
                                <form .edit-category-form .form-inline style="display:none;" method=post action=@{EditCategoryR $ entityKey category} enctype=#{enctype}>
                                    ^{formWidget}
                                    <button .btn.btn-primary type=submit name=action value=publish>_{MsgSave}
                                    <button .btn.btn-default.delete type=button name=action value=delete>_{MsgDelete}
                                    <a .btn.btn-default.cancel href=#>_{MsgCancel}
                            $if null entryList
                                <p style="margin-bottom:1.5em;">_{MsgNoPostInCategory} #
                                    $if isAuthor   
                                        <a href=@{EntriesR}>_{MsgBrowsePosts}.
                            $else
                                ^{entryListWidget "list-unstyled" entryList}

        |]
        subscribeToEntryWidget    
        toWidget [julius|
            $(document).ready(function() {
                $('.category-form .cancel').click(function() {
                    var categoryForm = $('.category-form');
                    $('.new-item').hide();
                    $('.new-entry').show();
                    return false;
                });
                $('.edit-category-form .cancel').click(function() {
                    var editCategoryForm = $(this).closest('form');
                    editCategoryForm.hide();
                    editCategoryForm.siblings().show();
                    return false;
                });
                $('.edit-category-form .delete').click(function() {
                    var editCategoryForm = $(this).closest('form');
                    if (confirm('Are you sure you want to delete this category?')) {
                        editCategoryForm.attr('method', 'delete');
                        editCategoryForm.submit();
                    }
                    return false;
                });
                $('.new-entry').click(function() {
                    var categoryForm = $('.category-form');
                    $('.new-entry').hide();//the button and link
                    $('.new-item').show();
                    categoryForm[0].reset();
                    return false;
                });

                $('.edit-entry>a').click(function() {  
                    var editCategoryForm = $(this).closest('.category').find('.edit-category-form');
                    editCategoryForm.siblings().hide();
                    editCategoryForm[0].reset();
                    editCategoryForm.show();
                    return false;
                });
                $('.category-form, .edit-category-form').submit(function() {
                    $.ajax({
                        type: $(this).attr('method'),
                        url: $(this).attr('action'),
                        data: $(this).serialize(),
                        success: function(data) {
                            if (data.errors) {
                                alert(data.errors);
                            } else {
                                location.reload();
                            }
                        }
                    });
                    return false;
                });

                $('#filter').change(function() {
                    var filter = $(this).val();
                    switch (filter) {
                        case 'own':
                            $('.entry-item').hide();
                            $('.by>a[href="@{UserHomeR authorId}"]').closest('.entry-item').show();
                            break;
                        case 'others':
                            $('.entry-item').show();
                            $('.by>a[href="@{UserHomeR authorId}"]').closest('.entry-item').hide();
                            break;
                        default:
                            $('.entry-item').show();
                    }
                });
            });|]    