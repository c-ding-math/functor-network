
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.EditPage where

import Import
import Yesod.Form.Bootstrap3
import Handler.Parser(parse,markItUpWidget)
import Parse.Parser (mdToHtml,texToHtml,EditorData(..))
--import qualified Data.Text as T
--import System.Directory

data EntryInput=EntryInput
    { preamble:: Maybe Textarea
    , inputFormat:: Format
    , content::Textarea 
    , citation:: Maybe Textarea}

defaultContent::Textarea
defaultContent=Textarea "Apparently, this user prefers to keep an air of mystery."

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
       
getEditPageR :: Text -> Handler Html
getEditPageR title = do
    (userId, user)<- requireAuthPair
    mAppUserId <- mAppAdministratorId
    let isAppAdministrator = mAppUserId==Just userId
    if title=="About" || isAppAdministrator
        then do
            mEntry<-runDB $ selectFirst [EntryInputTitle==.title,EntryType==.Page,EntryUserId==.userId] [Desc EntryInserted]
            formatParam <- lookupGetParam "format"
            let format = case (formatParam,mEntry) of
                    (Just "tex",_) -> Format "tex"
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
            <button .btn .btn-primary type=submit name=action value=publish>_{MsgUpdate}
            $if isAppAdministrator 
                <button .btn .btn-default type=submit name=action value=draft>_{MsgSave}
            <button .btn .btn-default .delete type=submit name=action value=delete>_{MsgReset}
                |]            
                toWidget        
                    [julius|
                        $(document).ready(function(){
                            $('.btn.delete').click(function(){
                                confirm("You are about to reset your about page! Are you really sure?");
                            });
                        });
                    |]
                
                markItUpWidget format (Format "html")
        else notFound


postEditPageR :: Text -> Handler Html
postEditPageR title = do
    userId <- requireAuthId
    mAppUserId <- mAppAdministratorId
    let isAppAdministrator = mAppUserId==Just userId
    mEntry<-runDB $ selectFirst [EntryInputTitle==.title,EntryType==.Page,EntryUserId==.userId] [Desc EntryInserted]
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
            bodyHtml<- liftIO $  parse (Just userId) parser editorData
            urlRenderParams<- getUrlRenderParams
            case entryAction of
                Just "delete"->case mEntry of 
                    Just (Entity entryId _)->do
                        runDB $ delete entryId 
                        if title=="About"
                            then do
                                setMessageI MsgAboutPageReset
                            else do
                                setMessageI MsgPageReset
                        setMessageI MsgAboutPageReset
                        redirect $ EditPageR title
                    Nothing->do
                        if title=="About"
                            then do
                                setMessageI MsgAboutPageReset
                            else do
                                setMessageI MsgPageReset
                        
                        redirect $ EditPageR title
                Just "publish"-> case mEntry of 
                    Nothing -> do
                        _<-runDB $ insert $ Entry   
                            {entryParentId=Nothing
                            ,entryUserId=userId
                            ,entryType=Page
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
                            ,entryInputTags=[]
                            ,entryOutputTags=[]
                            }
                        if title=="About"
                            then do
                                setMessage $ [hamlet|<a href=@{UserAboutR userId}>Your profile page</a> has been updated.|] urlRenderParams
                            else do
                                setMessage $ [hamlet|Your page, <a href=@{PageR title}>#{title}</a>, has been updated.|] urlRenderParams
                        
                        redirect $ EditPageR title
                    Just (Entity entryId _) -> do
                        runDB $ update entryId
                            [EntryUserId=.userId
                            ,EntryStatus=.Publish
                            ,EntryInputPreamble=.(preamble formData)
                            ,EntryInputFormat=.(inputFormat formData)
                            ,EntryInputBody=.(content formData)
                            ,EntryOutputBody=.bodyHtml
                            ,EntryInputCitation=.(citation formData)
                            ,EntryUpdated=.currentTime
                            ]
                        if title=="About"
                            then do
                                setMessage $ [hamlet|<a href=@{UserAboutR userId}>Your profile page</a> has been updated.|] urlRenderParams
                            else do
                                setMessage $ [hamlet|Your page, <a href=@{PageR title}>#{title}</a>, has been updated.|] urlRenderParams
                        
                        redirect $ EditPageR title

                Just "draft"-> if isAppAdministrator then case mEntry of 
                        Nothing -> do
                            _<-runDB $ insert $ Entry   
                                {entryParentId=Nothing
                                ,entryUserId=userId
                                ,entryType=Page
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
                                ,entryInputTags=[]
                                ,entryOutputTags=[]
                                }

                            setMessage $ [hamlet|Your page, <a href=@{PageR title}>#{title}</a>, has been saved.|] urlRenderParams
                            
                            redirect $ EditPageR title

                        Just (Entity entryId _) -> do
                            runDB $ update entryId
                                [EntryUserId=.userId
                                ,EntryStatus=.Draft
                                ,EntryInputPreamble=.(preamble formData)
                                ,EntryInputFormat=.(inputFormat formData)
                                ,EntryInputBody=.(content formData)
                                ,EntryOutputBody=.bodyHtml
                                ,EntryInputCitation=.(citation formData)
                                ,EntryUpdated=.currentTime
                                ]

                            setMessage $ [hamlet|Your page, <a href=@{PageR title}>#{title}</a>, has been saved.|] urlRenderParams
                            
                            redirect $ EditPageR title
                    else do
                        setMessageI  MsgSomethingWrong
                        redirect $ EditPageR title

                _-> do 
                    setMessageI  MsgSomethingWrong
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
