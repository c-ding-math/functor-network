{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Settings where

import Import
import Yesod.Auth.Email (registerR,forgotPasswordR,setpassR)
import Yesod.Form.Bootstrap3

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


data NameSetting=NameSetting{_name::Text}
data AboutSetting=AboutSetting{_about::Maybe Text}
--data AvatarSetting=AvatarSetting{_avatar::Maybe Text}
--data NotificationSetting=NotificationSetting{_notificationEmailId::Maybe EmailId}
    --deriving Show
data EmailSetting=EmailSetting{_email::Maybe Text}
data DefaultFormatSetting=DefaultFormatSetting{_defaultFormat::Format}
data DefaultPreambleSetting=DefaultPreambleSetting{_defaultPreamble::Maybe Textarea}
data DefaultCitationSetting=DefaultCitationSetting{_defaultCitation::Maybe Textarea}

nameSettingForm::User->Form NameSetting
nameSettingForm user=renderBootstrap3 BootstrapBasicForm $ NameSetting
    <$> areq textField (bfs MsgName) (Just(userName user))

aboutSettingForm::User->Form AboutSetting
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
{-avatarSettingForm::User->Form AvatarSetting
avatarSettingForm user=renderBootstrap3 BootstrapBasicForm $ AvatarSetting
    <$> aopt urlField (bfs MsgAvatar) (Just(userAvatar user))-}

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
        inputFormats = [("Markdown", Format "md"), ("LaTeX", Format "tex")]::[(Text, Format)]

defaultPreambleSettingForm::User->Form DefaultPreambleSetting
defaultPreambleSettingForm user=renderBootstrap3 BootstrapBasicForm $ DefaultPreambleSetting
    <$> aopt textareaField (bfs MsgDefaultPreamble) (Just(userDefaultPreamble user))

defaultCitationSettingForm::User->Form DefaultCitationSetting
defaultCitationSettingForm user=renderBootstrap3 BootstrapBasicForm $ DefaultCitationSetting
    <$> aopt textareaField (bfs MsgDefaultCitation) (Just(userDefaultCitation user))

getSettingsR :: Handler Html
getSettingsR = do 
    urlRender<-getUrlRender
    (userId, user)<-requireAuthPair
    (emails, googles) <- runDB $ do
        --roleEntities<-selectList [RoleUserId==.userId,RoleType==.Administrator] []  
        --mSites<- mapM (get . roleSiteId . entityVal) roleEntities   
        --sites<-selectList[SiteUserId==.Just userId] [Desc SiteInserted]
        emails<-selectList [EmailUserId==.Just userId, EmailVerified==.True] [Desc EmailInserted]
        googles<-selectList [ LoginPlugin==."google",LoginUserId==.Just userId, LoginVerified==.True] [Desc LoginInserted]
        --mNotification <-selectFirst [NotificationEmailId<-.map entityKey emails] [Desc NotificationInserted]
        return $ (emails, googles)

    (nameWidget, nameEnctype) <- generateFormPost $ nameSettingForm user
    (aboutWidget, aboutEnctype) <- generateFormPost $ aboutSettingForm user
    --(avatarWidget, avatarEnctype) <- generateFormPost $ avatarSettingForm user
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
                    <label>_{MsgLoginViaEmail}
                    <div>
                        $if null emails 
                            <a .btn .btn-default href=@{AuthR registerR}>_{MsgNewEmail}
                        $else
                            <ul>
                                $forall Entity emailId email<-emails
                                    <li>#{emailAddress email}
                                        <ul .entry-menu.inline-menu>
                                            <li>
                                                <a href=@{EmailSettingR emailId}>delete
                                            <li>    
                                                <a href=@{SubscriptionsR emailId}>manage subscriptions
                            <a .btn .btn-default href=@{AuthR registerR}>_{MsgNewEmail}
                    <p>
                
                    <label>_{MsgSignInWithGoogle}
                    <div>
                        $if null googles 
                            <a .btn .btn-default href=@{AuthR (PluginR "google" ["forward"])}>_{MsgNewGoogle}
                        $else
                            <ul>
                                $forall Entity googleId google<-googles
                                    <li>Google ID. #{drop 11 (loginIdent google)}
                                        <ul .entry-menu.inline-menu>
                                            <li>
                                                <a href=@{LoginSettingR googleId}>delete
                                        
                            <a .btn .btn-default href=@{AuthR (PluginR "google" ["forward"])}>_{MsgNewGoogle}
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
            <section .profile-setting>
                <h2>_{MsgProfile}
                <div>
                    <form .name-form method=post enctype=#{nameEnctype}>
                        ^{nameWidget}
                        <button .btn .btn-default type=submit name=setting value=name>_{MsgSave}
                    <p>
                <!--<div>
                    <div .avatar-container>
                        <div .avatar-left>
                            
                            <form method=post enctype=#{avatarEnctype}>
                                ^{avatarWidget}
                                <button .btn .btn-default type=submit name=setting value=avatar>_{MsgSave}
                        <div .avatar-right>
                            
                    <p>-->
                
                <div>
                    <form method=post enctype=#{aboutEnctype}>
                        ^{aboutWidget}
                        <p .note>You may either use the default about page on our platform or use your own about page somewhere else. In the latter case, please  provide the URL of your page in the blank and paste <code>#{relMeLink (urlRender $ UserHomeR userId) (userName user)}</code> in your page for ownership verification (You may replace <code>#{userName user}</code> with any text you like). 

                        <button .btn .btn-default type=submit name=setting value=about>_{MsgVerifyAndSave}

                        <a .btn .btn-default href=#{userAbout user}>_{MsgView}
                        
                    <p>
            <section .email-setting>
                <h2>_{MsgNotification} 
                <div>
                    <form .email-form method=post enctype=#{emailEnctype}>
                        ^{emailWidget}        
                        <p .note>_{MsgNotifyMeViaEmail "noreply@functor.network"}                    
                        <button .btn .btn-default type=submit name=setting value=email>_{MsgSave}
                    <p>
            <section .editor-setting>    
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
                        <a .btn .btn-default href=@{AuthR forgotPasswordR}>_{MsgForgotPassword}
                        <a .btn .btn-default href=@{AuthR setpassR}>_{MsgSetPassword}
                    <p>

        |]
        toWidget [lucius|
            section+section{
                margin-top:2em;
                border-top:1px solid #dce4ec;
            }

            .name-form,.email-form select,.format-form{
                max-width:15em;
            }
            .avatar-container{
                display:flex;
            }
            .avatar-right{
                width:15em;
                background-color:#eee;
            }
            .avatar-left{
                flex-grow:1;
            }
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
                    redirect $ SettingsR
                _ -> notFound
        Just "about"-> do
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
        {-Just "avatar"-> do
            ((result,_), _) <- runFormPost $ avatarSettingForm user
            case result of
                FormSuccess res -> do 
                    _<-runDB $ update userId [UserAvatar =. _avatar res]
                    setMessageI $ MsgChangeSaved
                    redirect $ SettingsR
                _ -> notFound -}
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

        

