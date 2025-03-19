{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Settings where

import Import
import Yesod.Auth.Email (registerR,forgotPasswordR,setpassR)
import Yesod.Form.Bootstrap3
import qualified Data.Text as T
import Handler.Files (uploadForm)

{-
import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor --(Cursor, attribute, attributeIs, element, fromDocument, ($//), (&//), (&/), content)

checkRelMeLink :: String -> Text ->  IO Bool
checkRelMeLink url targetUrl = do
    -- Download the HTML document
    doc <- simpleHttp url
    -- Parse the document
    -- reference: https://www.yesodweb.com/book/xml
    let cursor = fromDocument $ parseLBS doc
        hrefs = cursor $// element "a" >=> attributeIs "rel" "me" >=> attributeIs "href" targetUrl
    return $ not $ null $ hrefs
    
relMeLink:: Text -> Text -> Text 
relMeLink homeUrl text = "<a rel=\"me\" href=\""<>homeUrl<>"\">"<>text<>"</a>"
-}

data NameSetting=NameSetting{_name::Text}
--data AboutSetting=AboutSetting{_about::Maybe Text}
data AvatarSetting=AvatarSetting{_avatar::Maybe Text}
--data NotificationSetting=NotificationSetting{_notificationEmailId::Maybe EmailId}
    --deriving Show
data EmailSetting=EmailSetting{_email::Maybe Text}
data DefaultFormatSetting=DefaultFormatSetting{_defaultFormat::Format}
data DefaultPreambleSetting=DefaultPreambleSetting{_defaultPreamble::Maybe Textarea}
data DefaultCitationSetting=DefaultCitationSetting{_defaultCitation::Maybe Textarea}

nameSettingForm::User->Form NameSetting
nameSettingForm user=renderBootstrap3 BootstrapBasicForm $ NameSetting
    <$> areq nameField (bfs MsgName) (Just(userName user))
    where 
        maxNameLength=64
        nameField = check validateName textField
        validateName name = if T.length name <= maxNameLength then Right name else Left $ MsgTooLongByCharCount (T.length name - maxNameLength)

{-aboutSettingForm::User->Form AboutSetting
aboutSettingForm user=renderBootstrap3 BootstrapBasicForm $ AboutSetting
    <$> aopt urlField aboutSetting (Just (Just (userAbout user)))
    where 
        aboutSetting = FieldSettings
            { fsLabel = "About page"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "about"
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "Leave it blank if you want to use the default about page.")
                ]
            }
-}
avatarSettingForm::User->Form AvatarSetting
avatarSettingForm user=renderBootstrap3 BootstrapBasicForm $ AvatarSetting
    <$> aopt urlField (bfs MsgAvatar) (Just(userAvatar user))

{-notificationSettingForm:: [Entity Email]->Maybe EmailId->Form NotificationSetting
notificationSettingForm maybeEmailEntities maybeEmailId= renderBootstrap3 BootstrapBasicForm $ NotificationSetting
        <$> aopt (selectFieldList (options maybeEmailEntities)) (bfs MsgEmailNotification) (Just maybeEmailId)
        where 
            options::[Entity Email] -> [(Text, EmailId)]
            options entities =  (\(Entity eid e)->(emailAddress e, eid)) <$> entities
-}            

{-notificationSettingForm userId extra=do
        (res, view) <- mreq checkBoxField (fieldSettingsLabel MsgNotifyMeViaEmail) Nothing

        let result =  NotificationSetting <$> res
        let widget = do
              [whamlet|
                  #{extra}
                  <div>
                      ^{fvInput view} _{MsgNotifyMeViaEmail}
              |]

        return (result, widget)
-}
emailSettingForm::User->[Text]->Form EmailSetting
emailSettingForm user emails=renderBootstrap3 BootstrapBasicForm $ EmailSetting
    <$> aopt (selectFieldList options) (bfs MsgEmail) (Just(userEmail user))
    where
        options = [(email,email)|email<-emails]::[(Text, Text)]

defaultFormatSettingForm::User->Form DefaultFormatSetting
defaultFormatSettingForm user=renderBootstrap3 BootstrapBasicForm $ DefaultFormatSetting
    <$> areq (selectFieldList inputFormats) (bfs MsgDefaultFormat) (Just(userDefaultFormat user))
    where
        inputFormats = [(MsgMarkdownWithLaTeX, Format "md"), (MsgPureLaTeX, Format "tex")]

defaultPreambleSettingForm::User->Form DefaultPreambleSetting
defaultPreambleSettingForm user=renderBootstrap3 BootstrapBasicForm $ DefaultPreambleSetting
    <$> aopt textareaField (bfs MsgDefaultPreamble) (Just(userDefaultPreamble user))

defaultCitationSettingForm::User->Form DefaultCitationSetting
defaultCitationSettingForm user=renderBootstrap3 BootstrapBasicForm $ DefaultCitationSetting
    <$> aopt textareaField (bfs MsgDefaultCitation) (Just(userDefaultCitation user))

getSettingsR :: Handler Html
getSettingsR = do 
    --urlRender<-getUrlRender
    (userId, user)<-requireAuthPair
    (emails, googles, orcids) <- runDB $ do
        --roleEntities<-selectList [RoleUserId==.userId,RoleType==.Administrator] []  
        --mSites<- mapM (get . roleSiteId . entityVal) roleEntities   
        --sites<-selectList[SiteUserId==.Just userId] [Desc SiteInserted]
        emails<-selectList [EmailUserId==.Just userId, EmailVerified==.True] [Desc EmailInserted]
        googles<-selectList [ LoginPlugin==."google",LoginUserId==.Just userId, LoginVerified==.True] [Desc LoginInserted]
        orcids<-selectList [ LoginPlugin==."orcid",LoginUserId==.Just userId, LoginVerified==.True] [Desc LoginInserted]
        --mNotification <-selectFirst [NotificationEmailId<-.map entityKey emails] [Desc NotificationInserted]
        return $ (emails, googles,orcids)

    (nameWidget, nameEnctype) <- generateFormPost $ nameSettingForm user
    --(aboutWidget, aboutEnctype) <- generateFormPost $ aboutSettingForm user
    (avatarWidget, avatarEnctype) <- generateFormPost $ avatarSettingForm user
    (uploadFileWidget, uploadFileEnctype) <- generateFormPost uploadForm
    --(notificationWidget, notificationEnctype) <- generateFormPost $ notificationSettingForm emails (notificationEmailId . entityVal <$> mNotification)
    (formatWidget, formatEnctype) <- generateFormPost $ defaultFormatSettingForm user
    (emailWidget, emailEnctype) <- generateFormPost $ emailSettingForm user (emailAddress . entityVal <$> emails)
    (preambleWidget, preambleEnctype) <- generateFormPost $ defaultPreambleSettingForm user
    (citationWidget, citationEnctype) <- generateFormPost $ defaultCitationSettingForm user
    
    defaultLayout $ do   
        --addStylesheetRemote "https://cdn.jsdelivr.net/npm/bootstrap@5.2.3/dist/css/bootstrap.min.css"     
        [whamlet|
            
            <section .login-setting>
                <h2>_{MsgLogin}
                <div>
                    <label>_{MsgEmail}
                    <div>
                        $if not $ null emails 
                            <ul>
                                $forall Entity emailId email<-emails
                                    <li>#{emailAddress email}
                                      <span.menu>
                                        <ul.list-inline.text-lowercase>
                                            <li>
                                                <a.text-muted href=@{EmailSettingR emailId}>delete
                                            <li>    
                                                <a.text-muted href=@{SubscriptionsR emailId}>manage subscriptions
                        <a .btn .btn-default href=@{AuthR registerR}>_{MsgNewEmail}
                    <p>
                
                    <label>_{MsgSignInWithGoogle}
                    <div>
                        $if not $ null googles 
                            <ul>
                                $forall Entity googleId google<-googles
                                    <li>Google ID. #{drop 11 (loginIdent google)}
                                      <span.menu>  
                                        <ul.list-inline.text-lowercase>
                                            <li>
                                                <a.text-muted href=@{LoginSettingR googleId}>delete
                                        
                        <a .btn .btn-default href=@{AuthR (PluginR "google" ["forward"])}>_{MsgNewGoogle}
                    <p>

                    <label>_{MsgSignInWithORCID}
                    <div>
                        $if not $ null orcids
                            <ul>
                                $forall Entity pluginId pluginValue<-orcids
                                    <li>ORCID ID. #{T.replace "\"" "" (loginIdent pluginValue)}
                                      <span .menu>  
                                        <ul.list-inline.text-lowercase>
                                            <li>
                                                <a.text-muted href=@{LoginSettingR pluginId}>delete
                                        
                        <a .btn .btn-default href=@{AuthR (PluginR "orcid" ["forward"])}>_{MsgNewORCID}
                    <p>

            <!-- <section .site-setting>
                <h2>_{MsgSite}
                <div>
                    <label>_{MsgSitePath}
                    $if (null sites)
                        <p>
                            <a .btn .btn-default href=@{NewSiteR}>_{MsgRegisterSite}
                    $else
                        
                        $forall Entity _ site<-sites
                                <p> <a href=@{SiteR (sitePath site)}>@{SiteR (sitePath site)}
                                <a .btn .btn-default href=@{EditSiteR (sitePath site)}>_{MsgEdit} 
                                <a .btn .btn-default href=@{SiteR (sitePath site)}>_{MsgView}
                <p> -->
            <section .profile-setting #profile-setting>
                <h2>_{MsgProfile}
                <div>
                    <form .name-form method=post enctype=#{nameEnctype}>
                        ^{nameWidget}
                        <button .btn .btn-default type=submit name=setting value=name>_{MsgSave}
                    <p>
                <div>
                    <label>_{MsgAvatar}
                    <p>
                        $maybe avatar <- userAvatar user
                            <img.avatar.img-rounded src=#{avatar} alt=#{userName user}>
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
                
                <div>
                    <label>_{MsgAbout}
                    <div>
                        <a .btn .btn-default href=@{EditUserAboutR}>_{MsgEdit}
                        <a .btn .btn-default href=@{UserHomeR userId}#about>_{MsgView}   
                    <p>
            <section .email-setting>
                <h2>_{MsgNotification} 
                <div>
                    <form .email-form method=post enctype=#{emailEnctype}>
                        ^{emailWidget}
                        $#<p .text-muted>_{MsgNotifyMeViaEmail "noreply@functor.network"}
                        <p .text-muted>Follow your new post and comment via the above email by default. To ensure that the notification can reach you, please add <code>noreply@functor.network</code> to the whitelist with your email provider.                  
                        <button .btn .btn-default type=submit name=setting value=email>_{MsgSave}
                        <a .btn .btn-default href=@{AuthR registerR}>_{MsgNewEmail}
                    <p>
            <section #editor-setting .editor-setting>    
                <h2>_{MsgEditor}
                <div>                    
                    <form .format-form method=post enctype=#{formatEnctype}>
                        ^{formatWidget}
                        <button .btn .btn-default type=submit name=setting value=format>_{MsgSave}
                    <p>
                <div>                    
                    <form method=post enctype=#{preambleEnctype}>
                        ^{preambleWidget}
                        <button .btn .btn-default type=submit name=setting value=preamble>_{MsgSave}
                    <p>
                <div>                    
                    <form method=post enctype=#{citationEnctype}>
                        ^{citationWidget}
                        <button .btn .btn-default type=submit name=setting value=citation>_{MsgSave}
                    <p>
            <section .account-setting>
                <h2>_{MsgAccount}
                <div>
                    <label>_{MsgID} 
                    #{toPathPiece userId}
                    <div>           
                        <a .btn .btn-default href=@{AccountR}>_{MsgDelete}
                    <p>
                <div>
                    <label>_{MsgPassword}
                    <div>
                        <a .btn .btn-default href=@{AuthR setpassR}>_{MsgResetPassword}
                    <p>

        |]
        toWidget [lucius|
            section+section{
                margin-top:2em;
                border-top:1px solid #ccc;
            }
            .menu ul{
                display:inline-block;
                margin-left:2em;
            }
            .name-form,.email-form select, .format-form select{
                max-width:15em;
            }

            .avatar{
                height:128px;
                max-width:100%;
            }
            textarea{
                min-width:520px;
            }
        |]
        toWidget [julius|
            $(function(){
                var questionSvg = '<svg style="height:1em; vertical-align:middle;" class="bi bi-question-circle-fill" viewBox="0 0 16 16" fill="currentColor" xmlns="http://www.w3.org/2000/svg"><path fill-rule="evenodd" d="M16 8A8 8 0 1 1 0 8a8 8 0 0 1 16 0zM6.57 6.033H5.25C5.22 4.147 6.68 3.5 8.006 3.5c1.397 0 2.673.73 2.673 2.24 0 1.08-.635 1.594-1.244 2.057-.737.559-1.01.768-1.01 1.486v.355H7.117l-.007-.463c-.038-.927.495-1.498 1.168-1.987.59-.444.965-.736.965-1.371 0-.825-.628-1.168-1.314-1.168-.901 0-1.358.603-1.358 1.384zm1.251 6.443c-.584 0-1.009-.394-1.009-.927 0-.552.425-.94 1.01-.94.609 0 1.028.388 1.028.94 0 .533-.42.927-1.029.927z"/></svg><!-- Copyright (c) 2011-2024 The Bootstrap Authors, Licensed under MIT (https://github.com/twbs/bootstrap/blob/main/LICENSE) -->';
                $(".format-form select").siblings("label").wrap("<div/>");
                $(".format-form select").after($("<a/>",{class:"text-muted", href: '@{EditHelpR "format"}', target: "_blank", html: questionSvg}).css("padding-left","0.5em").css("padding-right","0.5em")).css("display","inline-block");

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

postSettingsR :: Handler Html
postSettingsR = do
    (userId,user)<-requireAuthPair
    --currentTime<-liftIO $ getCurrentTime
    setting <- lookupPostParam "setting"
    case setting of 
        Just "name"-> do
            ((result,_), _) <- runFormPost $ nameSettingForm user
            case result of
                FormSuccess res -> do 
                    _<-runDB $ update userId [UserName =. _name res]
                    setMessageI $ MsgChangeSaved                 
                FormFailure errors -> do
                    msgRender <- getMessageRender
                    setMessage [shamlet|
                        $forall error <- errors
                            #{ msgRender error}
                        |]
                _ -> setMessageI MsgFormMissing
            redirect $ SettingsR

        {-Just "about"-> do
            ((result,_), _) <- runFormPost $ aboutSettingForm user
            case result of
                FormSuccess res -> do 
                    urlRender<-getUrlRender
                    let defaultAboutPage = urlRender $ UserPageR userId "About"
                    case _about res of 
                        Just url | url /= defaultAboutPage ->do
                            eResult <- liftIO $ try $ checkRelMeLink (unpack $ url) (urlRender $ UserHomeR userId) :: Handler (Either SomeException Bool)
                            case eResult of
                                Left _ -> setMessageI $ MsgVerificationNoResponse
                                Right verified ->if verified
                                    then do
                                        _<-runDB $ update userId [UserAbout =. url]
                                        setMessageI $ MsgChangeSaved
                                        
                                    else do
                                        setMessageI $ MsgVerificationNoRelMeLink
                                        
                        _->do
                            _<-runDB $ update userId [UserAbout =. defaultAboutPage]
                            setMessageI $ MsgChangeSaved
                    redirect $ SettingsR
                _ -> notFound
        -}
        Just "avatar"-> do
            ((result,_), _) <- runFormPost $ avatarSettingForm user
            case result of
                FormSuccess res -> do 
                    _<-runDB $ update userId [UserAvatar =. _avatar res]
                    setMessageI $ MsgChangeSaved
                    redirect $ SettingsR
                _ -> notFound
        {-Just "notification"-> do
            (emails, selected) <- runDB $ do
                emails<-selectList [EmailUserId==.Just userId, EmailVerified==.True] [Desc EmailInserted]
                mNotification<-selectFirst[NotificationEmailId<-.(entityKey <$> emails)] [Desc NotificationInserted]
                return (emails,notificationEmailId . entityVal <$> mNotification)
            ((result,_), _) <- runFormPost $ notificationSettingForm emails selected
            
            case result of
                FormSuccess res -> do 
                    
                        
                        case _notificationEmailId res of 
                            Just eid->do
                                _<-runDB $ do
                                    deleteWhere [NotificationEmailId<-.(entityKey <$> emails)]
                                    _<-insert $ Notification{notificationType=EntrySubscription, notificationEmailId=eid, notificationInserted=currentTime}
                                    insert $ Notification{notificationType=CommentSubscription, notificationEmailId=eid, notificationInserted=currentTime}
                                setMessageI $ MsgChangeSaved
                                redirect $ SettingsR
                            _ -> do 
                                runDB $ do
                                    deleteWhere [NotificationEmailId<-.(entityKey <$> emails)]
                                --setMessage [shamlet|#{show emails} |]
                                setMessageI $ MsgChangeSaved
                                redirect $ SettingsR
                FormMissing-> notFound
                FormFailure errors -> do
                    setMessage [shamlet|
                        $forall error <- errors
                            <p>#{error}
                        |]
                    redirect $ SettingsR

        -}
        Just "format"-> do
            ((result,_), _) <- runFormPost $ defaultFormatSettingForm user
            case result of
                FormSuccess res -> do 
                    _<-runDB $ update userId [UserDefaultFormat =. _defaultFormat res]
                    setMessageI $ MsgChangeSaved
                    redirect $ SettingsR
                _ -> notFound
        Just "email"-> do
            emails <- runDB $ selectList [EmailUserId==.Just userId, EmailVerified==.True] [Desc EmailInserted]
            ((result,_), _) <- runFormPost $ emailSettingForm user (emailAddress . entityVal <$> emails)
            case result of
                FormSuccess res -> do 
                    _<-runDB $ update userId [UserEmail =. _email res]
                    setMessageI $ MsgChangeSaved
                    redirect $ SettingsR
                _ -> notFound
        Just "preamble"-> do
            ((result,_), _) <- runFormPost $ defaultPreambleSettingForm user
            case result of
                FormSuccess res -> do 
                    _<-runDB $ update userId [UserDefaultPreamble =. _defaultPreamble res]
                    setMessageI $ MsgChangeSaved
                    redirect $ SettingsR
                _ -> notFound
        Just "citation"-> do
            ((result,_), _) <- runFormPost $ defaultCitationSettingForm user
            case result of
                FormSuccess res -> do 
                    _<-runDB $ update userId [UserDefaultCitation =. _defaultCitation res]
                    setMessageI $ MsgChangeSaved
                    redirect $ SettingsR
                _ -> notFound

        Just x->invalidArgs [x]
        _->notFound

        

