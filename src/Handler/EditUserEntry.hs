
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
import Handler.Parser(parse,editorWidget,userTemporaryDirectory)
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
        inputFormats = [(MsgMarkdownWithLaTeX, Format "md"), (MsgPureLaTeX, Format "tex")]
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
    mEntry <- runDB $ selectFirst [EntryUserId ==.userId,EntryType==.UserPost] [Desc EntryInserted]
    entryInput <- runDB $ do 
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
    <div .text-left> 
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
                        }else if (title.length>0){
                            var progressBar=$('<div class="progress" style="width:38%;height:0.5em;"><div class="progress-bar progress-bar-striped active" role="progressbar" aria-valuenow="100" aria-valuemin="0" aria-valuemax="100" style="width:100%"></div></div>');
                            $('<div/>',{style:"position: fixed;top: 0;right: 0;bottom: 0;left: 0;display: flex;justify-content: center;align-items: center;z-index: 2;"}).append(progressBar).appendTo('body');
                            $('<div/>',{style:"position: fixed;top: 0;right: 0;bottom: 0;left: 0;display: flex;justify-content: center;align-items: center;z-index: 1;background-color:#000;opacity:0.5;"}).appendTo('body');
                        }
                    });
                });
            |]
        editorWidget format
        -- instruction for first post
        when (isNothing mEntry) $ do
            toWidget [julius|
                $(document).ready(function(){
                    //show popovers on "post format", "meta" and "content" fields on click one by one
                    
                    var popovers = [
                        {
                            element: $('select[name="format"]'),
                            content: 'Choose the format of your post here. A comparison between formats can be found at the <a target=\"_blank\" href=\"@{EditHelpR "format"}\">format help page</a>.',
                            placement: 'right'
                        },
                        {
                            element: $('.editor-toolbar>ul>li:contains("Meta")'),
                            content: 'Click here to include your latex preamble and citation. You can also set defaults at the <a target=\"_blank\" href=\"@{SettingsR}#editor-setting\">settings page</a>.',
                            //content: 'Click the "meta" menu to include your latex preamble and citation data.',
                            placement: 'bottom'
                        },
                        {
                            element: $('.ace_editor'),
                            content: 'The main content of your document goes here. The shortcut for preview refresh is Ctrl+Enter (on Windows) or Cmd+Enter (on Mac).',
                            placement: 'bottom'
                        },
                        {
                            element: $('.editor-toolbar>ul>li:contains("Help")'),
                            content: 'More help can be found here. Refresh the page if you want to see the first-time instruction again.',
                            placement: 'bottom'
                        },
                    ];

                    popovers.forEach(function(popover){
                        popover.element.popover({
                            html:true,
                            placement:popover.placement,
                            content:popover.content,
                            trigger: 'manual'
                        });
                    });
                    var popoverContainer=$('<div/>',{style:"position:fixed;top:0;left:0;width:100%;height:100%;"}).appendTo('body');
                    
                    var i=1;
                    popovers[i-1].element.popover('show');
                    popoverContainer.click(function(){
                        if(i<popovers.length){
                            popovers[i-1].element.popover('destroy');
                            popovers[i].element.popover('show');
                            i++;
                        }else{
                            popovers[i-1].element.popover('destroy');
                            popoverContainer.remove();
                        }
                    });

                });
            |]


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
    <div .text-left> 
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
                        }else if (title.length>0){
                            var progressBar=$('<div class="progress" style="width:38%;height:0.5em;"><div class="progress-bar progress-bar-striped active" role="progressbar" aria-valuenow="100" aria-valuemin="0" aria-valuemax="100" style="width:100%"></div></div>');
                            $('<div/>',{style:"position: fixed;top: 0;right: 0;bottom: 0;left: 0;display: flex;justify-content: center;align-items: center;z-index: 2;"}).append(progressBar).appendTo('body');
                            $('<div/>',{style:"position: fixed;top: 0;right: 0;bottom: 0;left: 0;display: flex;justify-content: center;align-items: center;z-index: 1;background-color:#000;opacity:0.5;"}).appendTo('body');
                        }
                    });
                });
            |]
        editorWidget format

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
                            , entryFeatured=isFeatured bodyHtml
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
                    setMessage $ [hamlet|
                                    Your post, #
                                    <a .alert-link href=@{UserEntryR userId entryId}>#{inputTitle formData}
                                    , has been published. #
                                    <a .alert-link.pull-right href=@{UserEntryR userId entryId}>View
                                 |] urlRenderParams
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
                            , entryFeatured=isFeatured bodyHtml
                            }
                        insertDefaultEntrySubscription entryId
                        return entryId
                    setMessage $ [hamlet|
                                    Your post, #
                                    <a .alert-link href=@{UserEntryR userId entryId}>#{inputTitle formData}
                                    , has been saved. #
                                    <a .alert-link.pull-right href=@{UserEntryR userId entryId}>View
                                 |] urlRenderParams
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
                        ,EntryFeatured=.isFeatured bodyHtml
                        ] 
                    setMessage $ [hamlet|
                                    Your post, #
                                    <a .alert-link href=@{UserEntryR userId entryId}>#{inputTitle formData}
                                    , has been published. #
                                    <a .alert-link.pull-right href=@{UserEntryR userId entryId}>View
                                 |] urlRenderParams --Message can't be too long, use title text instead of titleHtml
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
                            ,EntryFeatured=.isFeatured bodyHtml
                            ] 
                        
                    setMessage $ [hamlet|
                                    Your post, #
                                    <a .alert-link href=@{UserEntryR userId entryId}>#{inputTitle formData}
                                    , has been saved. #
                                    <a .alert-link.pull-right href=@{UserEntryR userId entryId}>View
                                 |] urlRenderParams
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

isFeatured :: Text -> Bool
isFeatured html = not $ any (`isInfixOf` html) ["lost proof","parser-message","filter-information"]