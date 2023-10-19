
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.EditPage0 where

import Import
import Yesod.Form.Bootstrap3
import Handler.Parser(parse,markItUpWidget,userTemporaryDirectory)
import Parse.Parser (mdToHtml,texToHtml,EditorData(..))
--import qualified Data.Text as T
--import System.Directory

data EntryInput=EntryInput
    { preamble:: Maybe Textarea
    , inputFormat:: Format
    , content::Textarea 
    , citation:: Maybe Textarea}

defaultContent::Textarea
defaultContent=Textarea ""

pageForm::Maybe EntryInput -> Html -> MForm Handler (FormResult EntryInput, Widget)
pageForm inputs=renderBootstrap3 BootstrapBasicForm $ EntryInput
    <$> aopt textareaField preambleSettings (preamble <$> inputs)
    <*> areq (selectFieldList inputFormats) "Content" (inputFormat <$> inputs)
    <*> areq textareaField editorSettings  (content  <$> inputs)
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
       
getEditPage0R :: Text -> Handler Html
getEditPage0R title = do
    (userId, user)<- requireAuthPair

    mEntry<-runDB $ selectFirst [EntryInputTitle==.title,EntryType==.Page0,EntryUserId==.Just userId] [Desc EntryInserted]
    formatParam <- lookupGetParam "format"
    let format = case (formatParam,mEntry) of
            (Just "tex",_) -> Format "tex"
            (Just "md",_) -> Format "md"
            (_,Just entry) -> entryInputFormat $ entityVal entry
            _ -> Format "md"

    (entryWidget, entryEnctype) <- case entityVal <$> mEntry of  
        Just entry-> do
            generateFormPost $ pageForm $ Just $ EntryInput (entryInputPreamble entry) format (entryInputBody entry) (entryInputCitation entry)
        Nothing->do 

            generateFormPost $ pageForm $ Just $ EntryInput (userDefaultPreamble user) format defaultContent (userDefaultCitation user)
    defaultLayout $ do
        setTitleI MsgEdit
        [whamlet|
<h1>#{title}
<form  method=post enctype=#{entryEnctype}>
    ^{entryWidget}
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
        
        markItUpWidget format (Format "html")


postEditPage0R :: Text -> Handler Html
postEditPage0R title = do
    userId <- requireAuthId
    mEntry<-runDB $ selectFirst [EntryInputTitle==.title,EntryType==.Page0,EntryUserId==.Just userId] [Desc EntryInserted]
    ((res, _), _) <- runFormPost $ pageForm Nothing
    case res of 
        FormSuccess formData->  do
            let editorData::EditorData
                editorData =  EditorData{
                            editorPreamble=preamble formData
                            ,editorContent=Just (content formData)
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
                        redirect $ EditPage0R title
                    Nothing->do
                        setMessageI MsgPageNotFound
                        redirect $ EditPage0R title
                Just "publish"-> case mEntry of 
                    Nothing -> do
                        _<-runDB $ insert $ Entry   
                            {entryUserId=Just userId
                            ,entryType=Page0
                            ,entryInputFormat=(inputFormat formData)
                            ,entryOutputFormat=Format "html"
                            ,entryInputTitle=title
                            ,entryOutputTitle=title
                            ,entryInputPreamble=(preamble formData)
                            ,entryInputBody=(content formData)
                            ,entryOutputBody=bodyHtml
                            ,entryInputCitation=(citation formData)
                            ,entryInserted=currentTime
                            ,entryUpdated=currentTime
                            --,entryStuck=Just currentTime
                            ,entryStatus=Publish
                            ,entryLocked=False
                            }

                        setMessage $ [hamlet|Your page, <a .alert-link href=@{Page0R title}>#{title}</a>, has been updated. <a .alert-link class=view href=@{Page0R title}>View</a>|] urlRenderParams
                           
                        redirect $ EditPage0R title
                    Just (Entity entryId _) -> do
                        runDB $ update entryId
                            [EntryUserId=.Just  userId
                            ,EntryStatus=.Publish
                            ,EntryInputPreamble=.(preamble formData)
                            ,EntryInputFormat=.(inputFormat formData)
                            ,EntryInputBody=.(content formData)
                            ,EntryOutputBody=.bodyHtml
                            ,EntryInputCitation=.(citation formData)
                            ,EntryUpdated=.currentTime
                            ]
                        setMessage $ [hamlet|Your page, <a .alert-link href=@{Page0R title}>#{title}</a>, has been updated. <a .alert-link class=view href=@{Page0R title}>View</a>|] urlRenderParams
                        
                        redirect $ EditPage0R title

                Just "draft"-> case mEntry of 
                        Nothing -> do
                            _<-runDB $ insert $ Entry   
                                {entryUserId=Just userId
                                ,entryType=Page0
                                ,entryInputFormat=(inputFormat formData)
                                ,entryOutputFormat=Format "html"
                                ,entryInputTitle=title
                                ,entryOutputTitle=title
                                ,entryInputPreamble=(preamble formData)
                                ,entryInputBody=(content formData)
                                ,entryOutputBody=bodyHtml
                                ,entryInputCitation=(citation formData)
                                ,entryInserted=currentTime
                                ,entryUpdated=currentTime
                                --,entryStuck=Just currentTime
                                ,entryStatus=Draft
                                ,entryLocked=False
                                }

                            setMessage $ [hamlet|Your page, <a .alert-link href=@{Page0R title}>#{title}</a>, has been saved. <a .alert-link class=view href=@{Page0R title}>View</a>|] urlRenderParams
                            
                            redirect $ EditPage0R title

                        Just (Entity entryId _) -> do
                            runDB $ update entryId
                                [EntryUserId=.Just  userId
                                ,EntryStatus=.Draft
                                ,EntryInputPreamble=.(preamble formData)
                                ,EntryInputFormat=.(inputFormat formData)
                                ,EntryInputBody=.(content formData)
                                ,EntryOutputBody=.bodyHtml
                                ,EntryInputCitation=.(citation formData)
                                ,EntryUpdated=.currentTime
                                ]

                            setMessage $ [hamlet|Your page, <a .alert-link href=@{Page0R title}>#{title}</a>, has been saved. <a .alert-link class=view href=@{Page0R title}>View</a>|] urlRenderParams
                            
                            redirect $ EditPage0R title

                _-> do 
                    setMessageI  MsgInvalidAction
                    redirect $ EditPage0R title
        FormFailure errors -> do
            setMessage [shamlet|
                $forall error <- errors
                    <p>#{error}
                |]
            redirect $ EditPage0R title
        FormMissing -> do
            setMessageI MsgFormMissing
            redirect $ EditPage0R title

