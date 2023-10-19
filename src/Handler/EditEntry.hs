
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.EditEntry where

import Import
import Yesod.Form.Bootstrap3
import Handler.EditComment(deleteEntryRecursive)
import Handler.Parser(parse,markItUpWidget,userTemporaryDirectory)
import Parse.Parser(mdToHtml,mdToHtmlSimple,texToHtml,texToHtmlSimple,EditorData(..))
import qualified Data.Text as T
import Text.Shakespeare.Text

data EntryInput=EntryInput
    { title::Text
    , preamble:: Maybe Textarea
    , inputFormat::Format
    , content::Textarea 
    , citation:: Maybe Textarea}

entryForm::Maybe EntryInput -> Html -> MForm Handler (FormResult EntryInput, Widget)
entryForm inputs=renderBootstrap3 BootstrapBasicForm $ EntryInput
    <$> areq textField titleSetting (title <$> inputs)
    <*> aopt textareaField preambleSettings (preamble <$> inputs)
    <*> areq (selectFieldList inputFormats) "Content" (inputFormat <$> inputs)
    <*> areq textareaField editorSettings  (content  <$> inputs)
    <*> aopt textareaField citationSettings (citation  <$> inputs) where  
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
        titleSetting=FieldSettings 
            { fsLabel = "Title"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "title"
            , fsAttrs =[ ("class", "form-control"), ("placeholder", "your title")]}

getNewEntryR:: Handler Html
getNewEntryR =  do
    (userId,user) <- requireAuthPair
    formatParam <- lookupGetParam "format"
    let format = case formatParam of
            Just "tex" -> Format "tex"
            _ -> Format "md"
    entryInput <- runDB $ do 
        mEntry <- selectFirst [EntryUserId ==. Just userId,EntryType==.Standard] [Desc EntryInserted]
        mSmaplePost<- case mEntry of
            Just _ -> return Nothing
            Nothing -> do 
                case format of
                    Format "tex" -> selectFirst [EntryInputTitle==."a sample post written in latex",EntryType==.Page0,EntryStatus==.Publish] [Desc EntryInserted]  
                    _ -> selectFirst [EntryInputTitle==."a sample post written in markdown",EntryType==.Page0,EntryStatus==.Publish] [Desc EntryInserted]
        case mSmaplePost of
            Just (Entity _ entry) -> return $ EntryInput "" (entryInputPreamble entry) format (entryInputBody entry) (entryInputCitation entry)
            Nothing -> return $ EntryInput "" (userDefaultPreamble user) format "" (userDefaultCitation user)
    (entryWidget, entryEnctype) <- generateFormPost $ entryForm $ Just entryInput
    --(entryWidget, entryEnctype) <- generateFormPost $ entryForm $ Just $ EntryInput "" (userDefaultPreamble user) format "" (userDefaultCitation user) "" 
    defaultLayout $ do
        setTitleI MsgEdit
        [whamlet|
<form  method=post enctype=#{entryEnctype}>
    ^{entryWidget}
    <button .btn .btn-primary type=submit name=action value=publish>_{MsgPublishPost}
    <button .btn .btn-default type=submit name=action value=draft>_{MsgSaveDraft}
        |]
        markItUpWidget format (Format "html")


getEditEntryR :: EntryId -> Handler Html
getEditEntryR entryId = do
    --userId <- requireAuthId
    entry<-runDB $ do    
        entry<-get404 entryId
        if entryType entry==Standard
            then return entry
            else notFound
 
    formatParam <- lookupGetParam "format"
    let format = case formatParam of
            Just "tex" -> Format "tex"
            Just "md" -> Format "md"
            _ -> entryInputFormat entry
    (entryWidget, entryEnctype) <- generateFormPost $ entryForm $ Just $ EntryInput (entryInputTitle entry) (entryInputPreamble entry) format (entryInputBody entry) (entryInputCitation entry)
    defaultLayout $ do
        setTitleI MsgEdit
        [whamlet|
<form  method=post enctype=#{entryEnctype}>
    ^{entryWidget}
    <button .btn .btn-primary type=submit name=action value=publish>_{MsgPublishPost}
    <button .btn .btn-default type=submit name=action value=draft>_{MsgSaveDraft}
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
                });
            |]
        
        markItUpWidget format (Format "html")

postNewEntryR :: Handler Html
postNewEntryR = do
    (userId, user) <- requireAuthPair
    
    ((res, _), _) <- runFormPost $ entryForm Nothing
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
            userDir<-userTemporaryDirectory
            (titleHtml,bodyHtml) <- liftIO $ do
                let (parser,parserSimple)=  case inputFormat formData of
                        Format "tex" -> (texToHtml,texToHtmlSimple)
                        _ -> (mdToHtml,mdToHtmlSimple)               
                titleHtml <-parse userDir parserSimple (title formData) --liftIO $ mdToHtmlSimple $ title formData
                bodyHtml <- parse userDir parser editorData
                return (titleHtml,bodyHtml)
            
            urlRenderParams<- getUrlRenderParams
            case entryAction of
                Just "publish"-> do
                    entryId<-runDB $ insert $ Entry   
                        {entryUserId=Just userId
                        ,entryType=Standard
                        ,entryInputFormat=inputFormat formData
                        ,entryOutputFormat=Format "html"
                        ,entryInputTitle=(title formData)
                        ,entryOutputTitle=titleHtml
                        ,entryInputPreamble=(preamble formData)
                        ,entryInputBody=(content formData)
                        ,entryOutputBody=bodyHtml
                        ,entryInputCitation=(citation formData)
                        ,entryInserted=currentTime
                        ,entryUpdated=currentTime
                        ,entryStatus=Publish
                        ,entryLocked=False
                        }
                    subscribers <- runDB $ selectList [UserSubscriptionUserId ==. userId, UserSubscriptionVerified ==. True] []
                    forM_ subscribers $ \(Entity subscriptionId subscription) -> do
                        
                        let unsubscribeUrl= urlRenderParams (EditUserSubscriptionR subscriptionId) $ case userSubscriptionKey subscription of
                                Just key -> [("key", key)] 
                                Nothing -> []
                            entryUrl= urlRenderParams (EntryR userId entryId) []
                            subject= "New post by " ++ (userName user) ++ ": " ++ (title formData) 
                            emailText= [stext|
The author #{userName user} you subscribed to has published a new post: 

#{title formData}.

You can view the post at #{entryUrl}.

To unsubscribe, please visit #{unsubscribeUrl}.

#{appName}
                            |]
                            emailHtml= [shamlet|
<p>The author #{userName user} you subscribed to has published a new post:
<p>#{title formData}
<p><a href=#{entryUrl}>Go to view</a><span> | </span><a href=#{unsubscribeUrl}>Manage subscriptions</a>    
<p>#{appName}
                            |]
                        sendSystemEmail (userSubscriptionEmail subscription) subject emailText emailHtml
                    setMessage $ [hamlet|Your blog post, <a class=alert-link href=@{EntryR userId entryId}>#{title formData}</a>, has been published. <a class='view alert-link' href=@{EntryR userId entryId}>View</a>|] urlRenderParams
                    redirect $ EditEntryR entryId
                _-> do 
                    entryId<-runDB $ insert $ Entry   
                        {entryUserId=Just userId
                        ,entryType=Standard
                        ,entryInputFormat=inputFormat formData
                        ,entryOutputFormat=Format "html"
                        ,entryInputTitle=(title formData)
                        ,entryOutputTitle=titleHtml
                        ,entryInputPreamble=(preamble formData)
                        ,entryInputBody=(content formData)
                        ,entryOutputBody=bodyHtml
                        ,entryInputCitation=(citation formData)
                        ,entryInserted=currentTime
                        ,entryUpdated=currentTime
                        ,entryStatus=Draft
                        ,entryLocked=False
                        }
                    setMessage $ [hamlet|Your blog post, <a class=alert-link href=@{EntryR userId entryId}>#{title formData}</a>, has been saved. <a class='view alert-link' href=@{EntryR userId entryId}>View</a>|] urlRenderParams
                    redirect $ EditEntryR entryId
        FormMissing -> do
            setMessageI MsgFormMissing
            redirect $ NewEntryR
        FormFailure errors -> do
            setMessage [shamlet|
                $forall error <- errors
                    <p>#{error}
                |]
            redirect $ NewEntryR

postEditEntryR :: EntryId -> Handler Html
postEditEntryR  entryId = do
    (userId, user) <- requireAuthPair
    ((res, _), _) <- runFormPost $ entryForm Nothing
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
            userDir<-userTemporaryDirectory
            (titleHtml,bodyHtml) <- liftIO $ do
                let (parser,parserSimple)=  case inputFormat formData of
                        Format "tex" -> (texToHtml,texToHtmlSimple)
                        _ -> (mdToHtml,mdToHtmlSimple)
                titleHtml <-parse userDir parserSimple (title formData) --liftIO $ mdToHtmlSimple $ title formData
                bodyHtml <- parse userDir parser editorData
                return (titleHtml,bodyHtml)
            urlRenderParams<- getUrlRenderParams
            
            case entryAction of
                Just "delete"->  do
                    runDB $ deleteEntryRecursive entryId
                    setMessage $ [shamlet|Your blog post, #{title formData}, has been deleted.|] --getUrlRenderParams
                    redirect $ NewEntryR
                Just "publish"-> do
                    entry<-runDB $ get404 entryId
                    when (entryInserted entry == entryUpdated entry) $ do
                        subscribers <- runDB $ selectList [UserSubscriptionUserId ==. userId, UserSubscriptionVerified ==. True] []
                        forM_ subscribers $ \(Entity subscriptionId subscription) -> do
                            let unsubscribeUrl= urlRenderParams (EditUserSubscriptionR subscriptionId) $ case userSubscriptionKey subscription of
                                    Just key -> [("key", key)] 
                                    Nothing -> []
                                entryUrl= urlRenderParams (EntryR userId entryId) []
                                subject= "New post by " ++ (userName user) ++ ": " ++ (title formData) 
                                emailText= [stext|
The author #{userName user} you subscribed to has published a new post:

#{title formData}.

You can view the post at #{entryUrl}.
To unsubscribe, please visit #{unsubscribeUrl}.
#{appName}
                                |]
                                emailHtml= [shamlet|
<p>The author #{userName user} you subscribed to has published a new post:
<p>#{title formData}
<p><a href=#{entryUrl}>Go to view</a><span> | </span><a href=#{unsubscribeUrl}>Manage subscriptions</a>
<p>#{appName}
                                |]
                            sendSystemEmail (userSubscriptionEmail subscription) subject emailText emailHtml

                    runDB $ update entryId                    
                        [EntryUserId=.Just userId
                        ,EntryStatus=.Publish
                        ,EntryInputTitle=.(title formData)
                        ,EntryOutputTitle=.titleHtml
                        ,EntryInputPreamble=.(preamble formData)
                        ,EntryInputFormat=.inputFormat formData
                        ,EntryInputBody=.(content formData)
                        ,EntryOutputBody=.bodyHtml
                        ,EntryInputCitation=.(citation formData)
                        ,EntryUpdated=.currentTime
                        ]
                    setMessage $ [hamlet|Your blog post, <a class=alert-link href=@{EntryR userId entryId}>#{title formData}</a>, has been published. <a class='view alert-link' href=@{EntryR userId entryId}>View</a>|] urlRenderParams --Message can't be too long, use title text instead of titleHtml
                    redirect $ EditEntryR entryId
                _-> do 
                    
                    runDB  $ do 
                        entry<-get404 entryId
                        let updateTime=case entryStatus entry of
                                Draft -> entryUpdated entry
                                _ -> currentTime
                        update entryId                    
                            [EntryUserId=.Just userId
                            ,EntryStatus=.Draft
                            ,EntryInputTitle=.(title formData)
                            ,EntryOutputTitle=.titleHtml
                            ,EntryInputPreamble=.(preamble formData)
                            ,EntryInputFormat=.inputFormat formData
                            ,EntryInputBody=.(content formData)
                            ,EntryOutputBody=.bodyHtml
                            ,EntryInputCitation=.(citation formData)
                            ,EntryUpdated=.updateTime
                            ]
                    setMessage $ [hamlet|Your blog post, <a class=alert-link href=@{EntryR userId entryId}>#{title formData}</a>, has been saved. <a class='view alert-link' href=@{EntryR userId entryId}>View</a>|] urlRenderParams
                    redirect $ EditEntryR entryId
        FormMissing -> do          
            setMessageI MsgFormMissing
            redirect $ EditEntryR entryId
        FormFailure errors -> do
            setMessage [shamlet|
                $forall error <- errors
                    <p>#{error}
                |]
            redirect $ EditEntryR entryId
            
listToInput::[Text]->Text
listToInput =(intercalate ", ")

inputToList::Text->[Text]
inputToList =(filter (not . null)) . (map T.strip) . (T.splitOn ",") 
