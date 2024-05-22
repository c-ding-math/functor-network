
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.EditUserEntry (
    EntryInput(..),
    entry2Html,
    entryInputForm,
    getNewUserEntryR,
    getEditUserEntryR,
    postNewUserEntryR,
    postEditUserEntryR
)where

import Import
import Yesod.Form.Bootstrap3
import Handler.EditComment(deleteEntryRecursive)
import Handler.Parser(parse,markItUpWidget,userTemporaryDirectory)
import Handler.NewEntrySubscription(insertDefaultEntrySubscription)
import Handler.NewUserSubscription(userSubscriptionNotification)
import Parse.Parser(mdToHtml,mdToHtmlSimple,texToHtml,texToHtmlSimple,EditorData(..))
--import Text.Shakespeare.Text

data EntryInput=EntryInput
    { inputTitle::Text
    , inputFormat::Format
    , inputPreamble::Maybe Textarea
    , inputBody::Maybe Textarea
    , inputCitation::Maybe Textarea
    }

entry2Html::EntryInput->Handler (Text,Text)
entry2Html formData=do
    let editorData::EditorData
        editorData = EditorData
            {editorPreamble=inputPreamble formData
            ,editorContent=inputBody formData
            ,editorCitation=inputCitation formData
            }            
    userDir<-userTemporaryDirectory
    let (parser,parserSimple)=  case inputFormat formData of
            Format "tex" -> (texToHtml,texToHtmlSimple)
            _ -> (mdToHtml,mdToHtmlSimple)               
    titleHtml <-liftIO $ parse userDir parserSimple (inputTitle formData)
    bodyHtml <- liftIO $ parse userDir parser editorData
    return (titleHtml,bodyHtml)

entryInputForm:: Maybe EntryInput -> Html -> MForm Handler (FormResult EntryInput, Widget)
entryInputForm mEntryInput=renderBootstrap3 BootstrapBasicForm $ EntryInput
    <$> areq textField titleSetting (inputTitle <$> mEntryInput)
    <*> areq (selectFieldList inputFormats) formatSettings (inputFormat <$> mEntryInput)
    <*> aopt textareaField preambleSettings (inputPreamble <$> mEntryInput)
    <*> aopt textareaField editorSettings (inputBody <$> mEntryInput)
    <*> aopt textareaField citationSettings (inputCitation <$> mEntryInput) where  
        inputFormats = [(MsgMarkdown, Format "md"), (MsgLaTeX, Format "tex")]
        formatSettings =  FieldSettings
            { fsLabel = SomeMessage MsgBody
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "format"
            , fsAttrs =[("class", "input-sm form-control format-selector")]}
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
        titleSetting=FieldSettings 
            { fsLabel = SomeMessage MsgTitle
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "title"
            , fsAttrs =[ ("class", "form-control"), ("placeholder", "your title")]}

getNewUserEntryR:: Handler Html
getNewUserEntryR =  do
    (userId,user) <- requireAuthPair
    formatParam <- lookupGetParam "format"
    let format = case formatParam of
            Just "tex" -> Format "tex"
            Just "md" -> Format "md"
            _->userDefaultFormat user
    entryInput <- runDB $ do 
        mEntry <- selectFirst [EntryUserId ==.userId,EntryType==.UserPost] [Desc EntryInserted]
        mSamplePost<- case mEntry of
            Just _ -> return Nothing
            Nothing -> do  
                case format of
                    Format "tex" -> selectFirst [EntryTitle==."a sample post written in latex",EntryType==.Page,EntryStatus==.Publish] [Desc EntryInserted]  
                    _ -> selectFirst [EntryTitle==."a sample post written in markdown",EntryType==.Page,EntryStatus==.Publish] [Desc EntryInserted]
        case mSamplePost of
            Just (Entity _ entry) -> return $ EntryInput
                { inputTitle="First post"
                , inputFormat=entryFormat entry
                , inputPreamble=entryPreamble entry
                , inputBody=entryBody entry
                , inputCitation=entryCitation entry
                }
            Nothing -> do                
                return $ EntryInput
                    { inputTitle=""
                    , inputFormat=format
                    , inputPreamble=userDefaultPreamble user
                    , inputBody=Nothing
                    , inputCitation=userDefaultCitation user
                    }
    (inputWidget, inputEnctype) <- generateFormPost $ entryInputForm $ Just entryInput
    --(inputWidget, inputEnctype) <- generateFormPost $ inputForm $ Just $ Entry "" (userDefaultPreamble user) format "" (userDefaultCitation user) "" 
    defaultLayout $ do
        setTitleI MsgEdit
        [whamlet|
<form  method=post enctype=#{inputEnctype}>
    ^{inputWidget}
    <button .btn .btn-primary .publish type=submit name=action value=publish>_{MsgPublishPost}
    <button .btn .btn-default .draft type=submit name=action value=draft>_{MsgSaveDraft}
        |]
        toWidget        
            [julius|
                $(document).ready(function(){
                    $('.btn.publish, .btn.draft').click(function(){
                        var title= $("input[name='title']").val();
                        if(title.length>256){
                            alert("Title is too long. Please make it shorter.");
                            return false;
                        }
                    });
                });
            |]
        markItUpWidget format (Format "html")


getEditUserEntryR :: EntryId -> Handler Html
getEditUserEntryR entryId = do
    --userId <- requireAuthId
    entry<-runDB $ do    
        entry<-get404 entryId
        if entryType entry==UserPost
            then return entry
            else notFound
 
    formatParam <- lookupGetParam "format"
    let format = case formatParam of
            Just "tex" -> Format "tex"
            Just "md" -> Format "md"
            _ -> entryFormat entry
    (inputWidget, inputEnctype) <- generateFormPost $ entryInputForm $ Just $ EntryInput
        { inputTitle=entryTitle entry
        , inputFormat=format
        , inputPreamble=entryPreamble entry
        , inputBody=entryBody entry
        , inputCitation=entryCitation entry
        }

    defaultLayout $ do
        setTitleI MsgEdit
        [whamlet|
<form  method=post enctype=#{inputEnctype}>
    ^{inputWidget}
    <button .btn .btn-primary .publish type=submit name=action value=publish>_{MsgPublishPost}
    <button .btn .btn-default .draft type=submit name=action value=draft>_{MsgSaveDraft}
    <button .btn .btn-default .delete type=submit name=action value=delete>_{MsgDelete}
        |]
            
        toWidget        
            [julius|
                $(document).ready(function(){
                    $('.btn.delete').click(function(){
                        if(!confirm("You are about to delete your entry! Are you really sure?")){
                            return false;
                        };
                    });
                    $('.btn.publish, .btn.draft').click(function(){
                        var title= $("input[name='title']").val();
                        if(title.length>256){
                            alert("Title is too long. Please make it shorter.");
                            return false;
                        }
                    });
                });
            |]
        
        markItUpWidget format (Format "html")

postNewUserEntryR :: Handler Html
postNewUserEntryR = do
    (userId, user) <- requireAuthPair
    
    ((res, _), _) <- runFormPost $ entryInputForm Nothing
    case res of 
        FormSuccess formData->  do
            (titleHtml,bodyHtml) <- entry2Html formData
            currentTime <- liftIO getCurrentTime
            entryAction <- lookupPostParam "action"                   
            urlRenderParams <- getUrlRenderParams
            case entryAction of
                Just "publish"-> do
                    entryId<-runDB $ do
                        entryId<-insert $ Entry
                            { entryUserId=userId
                            , entryType=UserPost
                            , entryStatus=Publish
                            , entryInserted=currentTime
                            , entryUpdated=currentTime
                            , entryTitle=inputTitle formData
                            , entryPreamble=inputPreamble formData
                            , entryFormat=inputFormat formData
                            , entryBody=inputBody formData
                            , entryCitation=inputCitation formData
                            , entryTitleHtml=titleHtml
                            , entryBodyHtml=bodyHtml
                            }
                        insertDefaultEntrySubscription entryId
                        return entryId
                    subscribers <- runDB $ selectList [UserSubscriptionUserId ==. userId, UserSubscriptionVerified ==. True] []
                    forM_ subscribers $ \(Entity subscriptionId subscription) -> do
                        
                        let unsubscribeUrl= urlRenderParams (EditUserSubscriptionR subscriptionId) $ case userSubscriptionKey subscription of
                                Just key -> [("key", key)] 
                                Nothing -> []
                            entryUrl= urlRenderParams (UserEntryR userId entryId) []
                        sendAppEmail (userSubscriptionEmail subscription) $ userSubscriptionNotification unsubscribeUrl entryUrl (inputTitle formData) user
                    setMessage $ [hamlet|Your post, <a class=alert-link href=@{UserEntryR userId entryId}>#{inputTitle formData}</a>, has been published. <a class='view alert-link' href=@{UserEntryR userId entryId}>View</a>|] urlRenderParams
                    redirect $ EditUserEntryR entryId
                _-> do 
                    entryId<-runDB $ do
                        entryId<-insert $ Entry
                            { entryUserId=userId
                            , entryType=UserPost
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
                            }
                        insertDefaultEntrySubscription entryId
                        return entryId
                    setMessage $ [hamlet|Your post, <a class=alert-link href=@{UserEntryR userId entryId}>#{inputTitle formData}</a>, has been saved. <a class='view alert-link' href=@{UserEntryR userId entryId}>View</a>|] urlRenderParams
                    redirect $ EditUserEntryR entryId
        FormMissing -> do
            setMessageI MsgFormMissing
            redirect $ NewUserEntryR
        FormFailure errors -> do
            setMessage [shamlet|
                $forall error <- errors
                    <p>#{error}
                |]
            redirect $ NewUserEntryR

postEditUserEntryR :: EntryId -> Handler Html
postEditUserEntryR  entryId = do
    (userId, user) <- requireAuthPair
    ((res, _), _) <- runFormPost $ entryInputForm Nothing
    case res of 
        FormSuccess formData->  do

            (titleHtml,bodyHtml) <- entry2Html formData
            currentTime <- liftIO getCurrentTime
            urlRenderParams<- getUrlRenderParams
            entryAction <- lookupPostParam "action"  
            case entryAction of
                Just "delete"->  do
                    runDB $ deleteEntryRecursive entryId
                    setMessage $ [shamlet|Your post, #{inputTitle formData}, has been deleted.|] --getUrlRenderParams
                    redirect $ NewUserEntryR
                Just "publish"-> do
                    entry<-runDB $ get404 entryId
                    when (entryInserted entry == entryUpdated entry) $ do
                        subscribers <- runDB $ selectList [UserSubscriptionUserId ==. userId, UserSubscriptionVerified ==. True] []
                        forM_ subscribers $ \(Entity subscriptionId subscription) -> do
                            let unsubscribeUrl= urlRenderParams (EditUserSubscriptionR subscriptionId) $ case userSubscriptionKey subscription of
                                    Just key -> [("key", key)] 
                                    Nothing -> []
                                entryUrl= urlRenderParams (UserEntryR userId entryId) []
                            sendAppEmail (userSubscriptionEmail subscription) $ userSubscriptionNotification unsubscribeUrl entryUrl (inputTitle formData) user
                    runDB $ update entryId                    
                        [EntryStatus=.Publish
                        ,EntryUpdated=.currentTime
                        ,EntryTitle=.inputTitle formData
                        ,EntryPreamble=.inputPreamble formData
                        ,EntryFormat=.inputFormat formData
                        ,EntryBody=.inputBody formData
                        ,EntryCitation=.inputCitation formData
                        ,EntryTitleHtml=.titleHtml
                        ,EntryBodyHtml=.bodyHtml
                        ] 
                    setMessage $ [hamlet|Your post, <a class=alert-link href=@{UserEntryR userId entryId}>#{inputTitle formData}</a>, has been published. <a class='view alert-link' href=@{UserEntryR userId entryId}>View</a>|] urlRenderParams --Message can't be too long, use title text instead of titleHtml
                    redirect $ EditUserEntryR entryId
                _-> do 
                    
                    runDB  $ do 
                        entry<-get404 entryId
                        let updateTime=case entryStatus entry of
                                Draft -> entryUpdated entry
                                _ -> currentTime
                        update entryId                    
                            [EntryStatus=.Draft
                            ,EntryUpdated=.updateTime
                            ,EntryTitle=.inputTitle formData
                            ,EntryPreamble=.inputPreamble formData
                            ,EntryFormat=.inputFormat formData
                            ,EntryBody=.inputBody formData
                            ,EntryCitation=.inputCitation formData
                            ,EntryTitleHtml=.titleHtml
                            ,EntryBodyHtml=.bodyHtml
                            ] 
                        
                    setMessage $ [hamlet|Your post, <a class=alert-link href=@{UserEntryR userId entryId}>#{inputTitle formData}</a>, has been saved. <a class='view alert-link' href=@{UserEntryR userId entryId}>View</a>|] urlRenderParams
                    redirect $ EditUserEntryR entryId
        FormMissing -> do          
            setMessageI MsgFormMissing
            redirect $ EditUserEntryR entryId
        FormFailure errors -> do
            setMessage [shamlet|
                $forall error <- errors
                    <p>#{error}
                |]
            redirect $ EditUserEntryR entryId

