
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.EditPage where

import Import
import Yesod.Form.Bootstrap3
import Handler.Parse(parse,editorWidget,userTemporaryDirectory)
import Parse.Parser (mdToHtml,texToHtml,EditorData(..))
--import qualified Data.Text as T
--import System.Directory

data EntryInput=EntryInput
    { preamble:: Maybe Textarea
    , inputFormat:: Format
    , body::Maybe Textarea 
    , citation:: Maybe Textarea}

pageForm::Maybe EntryInput -> Html -> MForm Handler (FormResult EntryInput, Widget)
pageForm inputs=renderBootstrap3 BootstrapBasicForm $ EntryInput
    <$> aopt textareaField preambleSettings (preamble <$> inputs)
    <*> areq (selectFieldList inputFormats) "Content" (inputFormat <$> inputs)
    <*> aopt textareaField editorSettings  (body  <$> inputs)
    <*> aopt textareaField citationSettings (citation  <$> inputs)  
    where
        inputFormats = [("Markdown", Format "md"), ("LaTeX", Format "tex")]::[(Text, Format)] 
        editorSettings = FieldSettings
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "content"
            , fsAttrs =[ ("class", "editor form-control"), ("placeholder", "Your content goes here. Press [Ctrl + 0] to preview.")]}
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
       
getEditPageR :: Text -> Handler Html
getEditPageR title = do
    (userId, user)<- requireAuthPair

    mEntry<-runDB $ selectFirst [EntryTitle==.title,EntryType==.Page,EntryUserId==.userId] [Desc EntryInserted]
    formatParam <- lookupGetParam "format"
    let format = case (formatParam,mEntry) of
            (Just "tex",_) -> Format "tex"
            (Just "md",_) -> Format "md"
            (_,Just entry) -> entryFormat $ entityVal entry
            _ -> Format "md"

    (entryWidget, entryEnctype) <- case entityVal <$> mEntry of  
        Just entry-> do
            generateFormPost $ pageForm $ Just $ EntryInput (entryPreamble entry) format (entryBody entry) (entryCitation entry)
        Nothing->do 

            generateFormPost $ pageForm $ Just $ EntryInput (userDefaultPreamble user) format Nothing (userDefaultCitation user)
    defaultLayout $ do
        setTitleI MsgEdit
        [whamlet|
<h1>#{title}
<form  method=post enctype=#{entryEnctype}>
    ^{entryWidget}
    <div .text-left>
        <button .btn .btn-primary type=submit name=action value=publish>_{MsgPublish}
        <button .btn .btn-default type=submit name=action value=draft>_{MsgSave}
        $maybe _ <- mEntry
            <button .btn .btn-default .delete type=submit name=action value=delete>_{MsgDelete}
        |]            
        toWidget        
            [julius|
                $(document).ready(function(){
                    $('.btn.delete').click(function(){
                        confirm("You are about to delete the page! Are you really sure?");
                    });
                });
            |]
        
        editorWidget format


postEditPageR :: Text -> Handler Html
postEditPageR title = do
    userId <- requireAuthId
    mEntry<-runDB $ selectFirst [EntryTitle==.title,EntryType==.Page,EntryUserId==.userId] [Desc EntryInserted]
    ((res, _), _) <- runFormPost $ pageForm Nothing
    case res of 
        FormSuccess formData->  do
            let editorData::EditorData
                editorData =  EditorData{
                            editorPreamble=preamble formData
                            ,editorContent=body formData
                            ,editorCitation=citation formData
                        } 
            entryAction <- lookupPostParam "action"        
            currentTime <- liftIO getCurrentTime
            let parser=case inputFormat formData of
                    Format "tex"->texToHtml
                    _->mdToHtml
            userDir<-userTemporaryDirectory
            bodyHtml<- liftIO $  parse userDir parser editorData
            urlRenderParams<- getUrlRenderParams
            case entryAction of
                Just "delete"->case mEntry of 
                    Just (Entity entryId _)->do
                        runDB $ delete entryId
                        setMessage $ [hamlet|Your page, #{title}, has been deleted.|] urlRenderParams
                        redirect $ EditPageR title
                    Nothing->do
                        setMessageI MsgPageNotFound
                        redirect $ EditPageR title
                Just "publish"-> case mEntry of 
                    Nothing -> do
                        _<-runDB $ insert $ Entry   
                            {entryUserId=userId
                            ,entryType=Page
                            ,entryFormat=(inputFormat formData)
                            ,entryTitle=title
                            ,entryPreamble=(preamble formData)
                            ,entryBody=(body formData)
                            ,entryCitation=(citation formData)
                            ,entryInserted=currentTime
                            ,entryUpdated=currentTime
                            ,entryStatus=Publish
                            ,entryTitleHtml=title
                            ,entryBodyHtml=bodyHtml
                            ,entryFeatured=False
                            }

                        setMessage $ [hamlet|
                                        Your page, #
                                        <a .alert-link href=@{PageR title}>#{title}
                                        , has been updated. #
                                        <a .alert-link.pull-right href=@{PageR title}>View
                                     |] urlRenderParams
                           
                        redirect $ EditPageR title
                    Just (Entity entryId _) -> do
                        runDB $ update entryId
                            [EntryUserId=.userId
                            ,EntryStatus=.Publish
                            ,EntryPreamble=.(preamble formData)
                            ,EntryFormat=.(inputFormat formData)
                            ,EntryBody=.(body formData)
                            ,EntryCitation=.(citation formData)
                            ,EntryUpdated=.currentTime
                            ,EntryBodyHtml=.bodyHtml
                            ]
                        setMessage $ [hamlet|
                                        Your page, #
                                        <a .alert-link href=@{PageR title}>#{title}
                                        , has been updated. #
                                        <a .alert-link.pull-right href=@{PageR title}>View
                                     |] urlRenderParams
                        
                        redirect $ EditPageR title

                Just "draft"-> case mEntry of 
                        Nothing -> do
                            _<-runDB $ insert $ Entry   
                                {entryUserId=userId
                                ,entryType=Page
                                ,entryFormat=(inputFormat formData)
                                ,entryTitle=title
                                ,entryPreamble=(preamble formData)
                                ,entryBody=(body formData)
                                ,entryCitation=(citation formData)
                                ,entryInserted=currentTime
                                ,entryUpdated=currentTime
                                ,entryStatus=Draft
                                ,entryTitleHtml=title
                                ,entryBodyHtml=bodyHtml
                                ,entryFeatured=False
                                }

                            setMessage $ [hamlet|
                                            Your page, #
                                            <a .alert-link href=@{PageR title}>#{title}
                                            , has been saved. #
                                            <a .alert-link.pull-right href=@{PageR title}>View
                                         |] urlRenderParams
                            
                            redirect $ EditPageR title

                        Just (Entity entryId _) -> do
                            runDB $ update entryId
                                [EntryUserId=.userId
                                ,EntryStatus=.Draft
                                ,EntryPreamble=.(preamble formData)
                                ,EntryFormat=.(inputFormat formData)
                                ,EntryBody=.(body formData)
                                ,EntryCitation=.(citation formData)
                                ,EntryUpdated=.currentTime
                                ,EntryBodyHtml=.bodyHtml
                                ]

                            setMessage $ [hamlet|
                                            Your page, #
                                            <a .alert-link href=@{PageR title}>#{title}
                                            , has been saved. #
                                            <a .alert-link.pull-right href=@{PageR title}>View
                                         |] urlRenderParams
                            
                            redirect $ EditPageR title

                _-> do 
                    setMessageI  MsgInvalidAction
                    redirect $ EditPageR title
        FormFailure errors -> do
            setMessage [shamlet|
                $forall error <- errors
                    <p>#{error}
                |]
            redirect $ EditPageR title
        FormMissing -> do
            setMessageI MsgFormMissing
            redirect $ EditPageR title

