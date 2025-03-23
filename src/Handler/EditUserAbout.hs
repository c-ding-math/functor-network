{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.EditUserAbout where

import Import
import Yesod.Form.Bootstrap3
import Handler.Parse(editorWidget)
import Handler.EditUserEntry(EntryInput(..),entry2Html)

entryInputForm:: Maybe EntryInput -> Html -> MForm Handler (FormResult EntryInput, Widget)
entryInputForm mEntryInput=renderBootstrap3 BootstrapBasicForm $ EntryInput
    <$> areq textField titleSetting (inputTitle <$> mEntryInput)
    <*> areq (selectFieldList inputFormats) formatSettings (inputFormat <$> mEntryInput)
    <*> aopt textareaField preambleSettings (inputPreamble <$> mEntryInput)
    <*> aopt textareaField editorSettings (inputBody <$> mEntryInput)
    <*> aopt textareaField citationSettings (inputCitation <$> mEntryInput) where  
        inputFormats = [(MsgMarkdownWithLaTeX, Format "md"), (MsgPureLaTeX, Format "tex")]
        formatSettings =  FieldSettings
            { fsLabel = SomeMessage MsgSelfIntroduction
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "format"
            , fsAttrs =[("class", "input-sm form-control format-selector")]}
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
        titleSetting=FieldSettings 
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "title"
            , fsAttrs =[ ("class", "form-control hidden"), ("placeholder", "your title")]}


getEditUserAboutR :: Handler Html
getEditUserAboutR = do
    (Entity userId user) <- requireAuth
    msgRender <- getMessageRender
    mEntry<-runDB $ selectFirst [EntryType==.UserPage, EntryUserId==.userId] [Desc EntryInserted]
    formatParam <- lookupGetParam "format"
    let format = case (formatParam,mEntry) of
            (Just "tex",_) -> Format "tex"
            (Just "md",_) -> Format "md"
            (_,Just entry) -> entryFormat $ entityVal entry
            _ -> userDefaultFormat user

    (entryWidget, entryEnctype) <- case entityVal <$> mEntry of  
        Just entry-> do
            generateFormPost $ entryInputForm $ Just $ EntryInput (entryTitle entry) format (entryPreamble entry) (entryBody entry) (entryCitation entry)
        Nothing-> do 
            generateFormPost $ entryInputForm $ Just $ EntryInput "About" format (userDefaultPreamble user) (Just (Textarea $ msgRender MsgNoAbout)) (userDefaultCitation user)
    defaultLayout $ do
        setTitle "About"
        [whamlet|
            <h1>About
            <form  method=post enctype=#{entryEnctype}>
                ^{entryWidget}
                <div .text-left>
                    <button .btn .btn-primary type=submit name=action value=publish>_{MsgSave}
        |]
        editorWidget format

postEditUserAboutR :: Handler Html
postEditUserAboutR = do
    (Entity userId _) <- requireAuth
    ((res, _), _) <- runFormPost $ entryInputForm Nothing
    mEntry<-runDB $ selectFirst [EntryType==.UserPage, EntryUserId==.userId] [Desc EntryInserted]
    case res of
        FormSuccess formData -> do
            (titleHtml,bodyHtml)<-entry2Html formData
            currentTime <- liftIO getCurrentTime
            case mEntry of
                Just (Entity entryId _) -> do
                    runDB $ update entryId
                        [EntryStatus=.Draft
                        ,EntryUpdated=.currentTime
                        ,EntryTitle=.inputTitle formData
                        ,EntryPreamble=.inputPreamble formData
                        ,EntryFormat=.inputFormat formData
                        ,EntryBody=.inputBody formData
                        ,EntryCitation=.inputCitation formData
                        ,EntryTitleHtml=.titleHtml
                        ,EntryBodyHtml=.bodyHtml
                        ] 
                Nothing -> do
                    _<-runDB $ insert $ Entry
                        { entryUserId=userId
                        , entryType=UserPage
                        , entryStatus=Draft
                        , entryInserted=currentTime
                        , entryUpdated=currentTime
                        , entryTitle=inputTitle formData
                        , entryPreamble=inputPreamble formData
                        , entryFormat=inputFormat formData
                        , entryBody=inputBody formData
                        , entryCitation=inputCitation formData
                        , entryTitleHtml=titleHtml
                        , entryBodyHtml=bodyHtml
                        , entryFeatured=False
                        }
                    return ()
            setMessageI MsgChangeSaved
            redirect $ UserHomeR userId -- :#: ("about"::Text)
        FormFailure errors -> do
            setMessage [shamlet|
                $forall error <- errors
                    <p>#{error}
                |]
            redirect $ EditUserAboutR
        FormMissing -> do
            setMessageI MsgFormMissing
            redirect $ EditUserAboutR