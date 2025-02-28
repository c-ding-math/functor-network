{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.MaintainEntry where

import Import
import Yesod.Form.Bootstrap3
import Handler.EditComment(deleteEntryRecursive)
import Handler.Parser(editorWidget)
import Handler.EditUserEntry(EntryInput(..),entryInputForm,entry2Html)


maintainEntryForm :: Entry -> Form Entry
maintainEntryForm entry = renderBootstrap3 BootstrapBasicForm $ Entry
    <$> pure (entryUserId entry)
    <*> pure (entryInserted entry)
    <*> pure (entryUpdated entry)
    <*> pure (entryType entry)
    <*> pure (entryStatus entry)
    <*> areq textField titleSettings (Just (entryTitle entry))
    <*> pure (entryFormat entry)
    <*> aopt textareaField preambleSettings (Just (entryPreamble entry))
    <*> aopt textareaField editorSettings (Just (entryBody entry))
    <*> aopt textareaField citationSettings (Just (entryCitation entry))
    <*> areq textField (bfs ("titleHtml" :: Text)) (Just (entryTitleHtml entry))
    <*> areq textField (bfs ("bodyHtml" :: Text)) (Just (entryBodyHtml entry))
    <*> areq checkBoxField featuredSettings (Just (entryFeatured entry)) where
        {-inputFormats = [(MsgMarkdownWithLaTeX, Format "md"), (MsgPureLaTeX, Format "tex")]
        formatSettings =  FieldSettings
            { fsLabel = SomeMessage MsgBody
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "format"
            , fsAttrs =[("class", "input-sm form-control format-selector")]}-}
        editorSettings = FieldSettings
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "content"
            , fsAttrs =[ ("class", "editor form-control"), ("placeholder", "Your content goes here. Press [Ctrl + Enter] to preview.")]}
        preambleSettings = FieldSettings
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "preamble"
            , fsAttrs =[("class", "hidden")]}
        citationSettings = FieldSettings
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "citation"
            , fsAttrs =[("class", "hidden")]}
        titleSettings=FieldSettings 
            { fsLabel = SomeMessage MsgTitle
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "title"
            , fsAttrs =[ ("class", "form-control"), ("placeholder", "your title")]}
        featuredSettings=FieldSettings 
            { fsLabel = SomeMessage MsgFeatured
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "featured"
            , fsAttrs =[]}
 
getMaintainEntryR :: EntryId -> Handler Html
getMaintainEntryR entryId = do
    entry <- runDB $ get404 entryId

    entryIds' <- runDB $ selectKeysList [] [Asc EntryId]
    entryIds <- return $ filter (> entryId) entryIds'
    let mNextEntryId = minimumMay entryIds

    let format = entryFormat entry
    (formWidget, formEnctype) <- generateFormPost $ maintainEntryForm entry
    defaultLayout $ do
        setTitleI MsgMaintain
        [whamlet|
            <ul>
                <li>entryId: #{show entryId}
                <li>entryUserId: #{show $ entryUserId entry}
                <li>entryInserted: #{show $ entryInserted entry}
                <li>entryUpdated: #{show $ entryUpdated entry}
                <li>entryType: #{show $ entryType entry}
                <li>entryStatus: #{show $ entryStatus entry}

            <form method=post enctype=#{formEnctype}>
                <div .text-left>
                    <a .btn .btn-default .pull-right href=@{UserEntryR (entryUserId entry) entryId}>View
                    $maybe nextEntryId <- mNextEntryId
                        <a .btn .btn-default .pull-right .next href=@{MaintainEntryR nextEntryId}>Next
                    <button .btn .btn-default .save type=submit name=action value=save>_{MsgSave}
                ^{formWidget}
        |]
        editorWidget format

postMaintainEntryR :: EntryId -> Handler Html
postMaintainEntryR entryId = do
    entry <- runDB $ get404 entryId    
    ((result, formWidget), formEnctype) <- runFormPost $ maintainEntryForm entry
    case result of
        FormSuccess entry' -> do
            let editorData = EntryInput
                    { inputTitle = entryTitle entry'
                    , inputFormat = entryFormat entry'
                    , inputPreamble = entryPreamble entry'
                    , inputBody = entryBody entry'
                    , inputCitation = entryCitation entry'
                    }
            (titleHtml,bodyHtml) <- entry2Html editorData
            runDB $ update entryId
                [ EntryTitle =. entryTitle entry'
                , EntryFormat =. entryFormat entry'
                , EntryPreamble =. entryPreamble entry'
                , EntryBody =. entryBody entry'
                , EntryCitation =. entryCitation entry'
                , EntryTitleHtml =. titleHtml
                , EntryBodyHtml =. bodyHtml
                , EntryFeatured =. entryFeatured entry'
                ]
            redirect $ MaintainEntryR entryId
        _ -> defaultLayout $ do
            setTitleI MsgMaintain
            [whamlet|
                _{MsgError}
            |]
