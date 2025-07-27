{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.EditUserAbout where

import Import
import Yesod.Form.Bootstrap3
import Handler.Parse(editorWidget)
import Handler.EditUserEntry(EntryInput(..),entry2Html)
import Handler.Files (uploadForm)

data NameSetting=NameSetting{_name::Text}
data AvatarSetting=AvatarSetting{_avatar::Maybe Text}
nameSettingForm::User->Form NameSetting
nameSettingForm user=renderBootstrap3 BootstrapBasicForm $ NameSetting
    <$> areq nameField (bfs MsgName) (Just(userName user))
    where 
        maxNameLength=64
        nameField = check validateName textField
        validateName name = if length name <= maxNameLength then Right name else Left $ MsgTooLongByCharCount (length name - maxNameLength)

avatarSettingForm::User->Form AvatarSetting
avatarSettingForm user=renderBootstrap3 BootstrapBasicForm $ AvatarSetting
    <$> aopt urlField (bfs MsgAvatar) (Just(userAvatar user))

entryInputForm:: Maybe EntryInput -> Html -> MForm Handler (FormResult EntryInput, Widget)
entryInputForm mEntryInput=renderBootstrap3 BootstrapBasicForm $ EntryInput
    <$> areq textField titleSetting (inputTitle <$> mEntryInput)
    <*> areq (selectFieldList inputFormats) formatSettings (inputFormat <$> mEntryInput)
    <*> aopt textareaField preambleSettings (inputPreamble <$> mEntryInput)
    <*> aopt textareaField editorSettings (inputBody <$> mEntryInput)
    <*> aopt textareaField citationSettings (inputCitation <$> mEntryInput) where  
        inputFormats = [(MsgMarkdownWithLaTeX, Format "md"), (MsgPureLaTeX, Format "tex")]
        formatSettings =  FieldSettings
            { fsLabel = SomeMessage MsgAbout
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

    (nameWidget, nameEnctype) <- generateFormPost $ nameSettingForm user
    (avatarWidget, avatarEnctype) <- generateFormPost $ avatarSettingForm user
    (uploadFileWidget, uploadFileEnctype) <- generateFormPost uploadForm
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
            <div .page-header>
                <h1>_{MsgProfile}
            <div .profile>
                <div style="margin-bottom:2em;">
                    <label>_{MsgAvatar}
                    <p>
                        $maybe avatar <- userAvatar user
                            <img.avatar.img-rounded src=#{avatar} alt=#{userName user} width=128>
                        $nothing
                            <img.avatar.img-rounded src=@{StaticR $ StaticRoute ["icons","default-avatar.svg"] []} alt=#{userName user}>
                    <button type=button .btn .btn-default #avatar-button>_{MsgChangeAvatar}
                    <div .modal.fade #avatar-modal>
                        <div .modal-dialog>
                            <div .modal-content>
                                <div .modal-header>
                                    <button type=button .close data-dismiss=modal>&times;
                                    <b .modal-title>_{MsgChangeAvatar}
                                <div .modal-body>
                                    <form #file-form method=post enctype=#{uploadFileEnctype} action=@{FilesR}>
                                        ^{uploadFileWidget}
                                        <small .text-muted>_{MsgRecommendedFileSize}
                                        <div .text-right>
                                            <button .btn .btn-default type=submit>_{MsgUpload}        
                                    <form .hidden #avatar-form method=post enctype=#{avatarEnctype}>
                                        ^{avatarWidget}
                                        <button .btn .btn-default type=submit name=setting value=avatar>_{MsgSave}
                    <p>
                <div style="margin-bottom:2em;">
                    <form .name-form method=post enctype=#{nameEnctype} style="max-width:15em;">
                        ^{nameWidget}
                        <button .btn .btn-default type=submit name=setting value=name>_{MsgSave}
                    <p>                
            <form  method=post enctype=#{entryEnctype}>
                ^{entryWidget}
                <div .text-left>
                    <button .btn .btn-default type=submit name=action value=publish>_{MsgSave}
        |]
        editorWidget format
        toWidget [julius|
            $(function(){
                $("#avatar-button").click(function(){
                    $("#avatar-modal").modal("show");
                });
                $("#file-form button").click(function(e){
                    e.preventDefault();
                    var fileForm=$("#file-form");
                    var that=$(this);
                    var formData = new FormData(fileForm[0]);
                    $.ajax({
                        url: "@{FilesR}",
                        type: "POST",
                        processData: false,
                        contentType: false,
                        cache: false,
                        data: formData, 
                        beforeSend: function() {
                            that.prop("disabled", true);
                            that.addClass("loading");
                            fileForm.find(".form-group").removeClass("has-error").find(".help-block").remove();
                        },
                        error: function(jqXHR, textStatus, errorThrown) {
                            fileForm.find(".form-group").addClass("has-error").append('<span class="help-block">Error: ' + errorThrown + '</span>');
                        },
                        success: function(object, textStatus, jqXHR) {
                            var url = object.url;
                            $("#avatar-modal").modal("hide");
                            $("#avatar-form input[type='url']").val(url);
                            $("#avatar-form button").click();
                        },
                        complete: function() {
                            that.prop("disabled", false);
                            that.removeClass("loading");
                        },
                    });
                });
            });
        |]

postEditUserAboutR :: Handler Html
postEditUserAboutR = do
 (Entity userId user) <- requireAuth
 urlRenderParams<- getUrlRenderParams
 setting <- lookupPostParam "setting"
 case setting of
  Just "avatar" -> do
        ((result,_), _) <- runFormPost $ avatarSettingForm user
        case result of
            FormSuccess res -> do 
                _<-runDB $ update userId [UserAvatar =. _avatar res]
                setMessage $ [hamlet|
                    Profile updated. #
                    <a .alert-link.pull-right href=@{UserHomeR userId}>View
                    |] urlRenderParams
            FormFailure errors -> do
                msgRender <- getMessageRender
                setMessage [shamlet|
                    $forall error <- errors
                        #{ msgRender error}
                    |]
            _ -> setMessageI MsgFormMissing
        redirect $ EditUserAboutR
  Just "name"-> do
            ((result,_), _) <- runFormPost $ nameSettingForm user
            case result of
                FormSuccess res -> do 
                    _<-runDB $ update userId [UserName =. _name res]
                    setMessage $ [hamlet|
                        Profile updated. #
                        <a .alert-link.pull-right href=@{UserHomeR userId}>View
                        |] urlRenderParams               
                FormFailure errors -> do
                    msgRender <- getMessageRender
                    setMessage [shamlet|
                        $forall error <- errors
                            #{ msgRender error}
                        |]
                _ -> setMessageI MsgFormMissing
            redirect $ EditUserAboutR
  _ -> do  
    ((res, _), _) <- runFormPost $ entryInputForm Nothing
    mEntry<-runDB $ selectFirst [EntryType==.UserPage, EntryUserId==.userId] [Desc EntryInserted]
    case res of
        FormSuccess formData -> do
         if inputBody formData == Nothing
          then do
            setMessage $ [hamlet|
                    Please fill in some content. #
                    
                    |] urlRenderParams
            
          else do
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
            setMessage $ [hamlet|
                    Profile updated. #
                    <a .alert-link.pull-right href=@{UserHomeR userId}>View
                    |] urlRenderParams
            
        FormFailure errors -> do
            setMessage [shamlet|
                $forall error <- errors
                    <p>#{error}
                |]
            
        FormMissing -> do
            setMessageI MsgFormMissing
    redirect $ EditUserAboutR