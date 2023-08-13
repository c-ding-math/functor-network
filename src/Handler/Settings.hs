{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Settings where

import Import
import Yesod.Auth.Email (registerR,forgotPasswordR,setpassR)
import Yesod.Form.Bootstrap3

data NameSetting=NameSetting{_name::Text}
--data AvatarSetting=AvatarSetting{_avatar::Maybe Text}
data NotificationSetting=NotificationSetting{_notificationEmailId::Maybe EmailId}
    deriving Show
data DefaultPreambleSetting=DefaultPreambleSetting{_defaultPreamble::Maybe Textarea}
data DefaultCitationSetting=DefaultCitationSetting{_defaultCitation::Maybe Textarea}

nameSettingForm::User->Form NameSetting
nameSettingForm user=renderBootstrap3 BootstrapBasicForm $ NameSetting
    <$> areq textField (bfs MsgName) (Just(userName user))

{-avatarSettingForm::User->Form AvatarSetting
avatarSettingForm user=renderBootstrap3 BootstrapBasicForm $ AvatarSetting
    <$> aopt urlField (bfs MsgAvatar) (Just(userAvatar user))-}

notificationSettingForm:: [Entity Email]->Maybe EmailId->Form NotificationSetting
notificationSettingForm maybeEmailEntities maybeEmailId= renderBootstrap3 BootstrapBasicForm $ NotificationSetting
        <$> aopt (selectFieldList (options maybeEmailEntities)) (bfs MsgEmailNotification) (Just maybeEmailId)
        where 
            options::[Entity Email] -> [(Text, EmailId)]
            options entities =  (\(Entity eid e)->(emailAddress e, eid)) <$> entities
            

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
defaultPreambleSettingForm::User->Form DefaultPreambleSetting
defaultPreambleSettingForm user=renderBootstrap3 BootstrapBasicForm $ DefaultPreambleSetting
    <$> aopt textareaField (bfs MsgDefaultPreamble) (Just(userDefaultPreamble user))

defaultCitationSettingForm::User->Form DefaultCitationSetting
defaultCitationSettingForm user=renderBootstrap3 BootstrapBasicForm $ DefaultCitationSetting
    <$> aopt textareaField (bfs MsgDefaultCitation) (Just(userDefaultCitation user))

getSettingsR :: Handler Html
getSettingsR = do 
    (userId, user)<-requireAuthPair
    (emails, googles, mNotification) <- runDB $ do
        --roleEntities<-selectList [RoleUserId==.userId,RoleType==.Administrator] []  
        --mSites<- mapM (get . roleSiteId . entityVal) roleEntities   
        --sites<-selectList[SiteUserId==.Just userId] [Desc SiteInserted]
        emails<-selectList [EmailUserId==.Just userId, EmailVerified==.True] [Desc EmailInserted]
        googles<-selectList [ LoginPlugin==."google",LoginUserId==.Just userId, LoginVerified==.True] [Desc LoginInserted]
        mNotification <-selectFirst [NotificationEmailId<-.map entityKey emails] [Desc NotificationInserted]
        return $ (emails, googles, mNotification)

    (nameWidget, nameEnctype) <- generateFormPost $ nameSettingForm user
    --(avatarWidget, avatarEnctype) <- generateFormPost $ avatarSettingForm user
    --(notificationWidget, notificationEnctype) <- generateFormPost $ notificationSettingForm emails (notificationEmailId . entityVal <$> mNotification)
    (preambleWidget, preambleEnctype) <- generateFormPost $ defaultPreambleSettingForm user
    (citationWidget, citationEnctype) <- generateFormPost $ defaultCitationSettingForm user
    
    defaultLayout $ do   
        --addStylesheetRemote "https://cdn.jsdelivr.net/npm/bootstrap@5.2.3/dist/css/bootstrap.min.css"     
        [whamlet|
            
            <section .account-setting>
                <h2>_{MsgAccount}
                <div>
                    <label>_{MsgNumber}
                    #{toPathPiece userId}
                    <!--<label>_{MsgUserNumber}
                    <div>
                        <p>
                            <span>_{MsgNumber}#{toPathPiece userId}
                        
                        <a .btn .btn-default href=@{AccountR}>_{MsgEdit}-->
                    <p>
                <div>
                    <label>_{MsgPassword}
                    <div>
                        <a .btn .btn-default href=@{AuthR forgotPasswordR}>_{MsgForgotPassword}
                        <a .btn .btn-default href=@{AuthR setpassR}>_{MsgSetPassword}
                    <p>
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
                                        <a href=@{EmailSettingR emailId}>delete
                            <a .btn .btn-default href=@{AuthR registerR}>_{MsgNewEmail}
                    <p>
                
                    <label>_{MsgLoginViaGoogle}
                    <div>
                        $if null googles 
                            <a .btn .btn-default href=@{AuthR (PluginR "google" ["forward"])}>_{MsgNewGoogle}
                        $else
                            <ul>
                                $forall Entity googleId google<-googles
                                    <li>#{loginIdent google}
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
                    <label>_{MsgAbout}
                    <div>
                        <a .btn .btn-default href=@{PageR userId "About"}>_{MsgView}
                        <a .btn .btn-default href=@{EditPageR "About"}>_{MsgEdit}
                    <p>
            <!--<section .notification-setting>
                <h2>_{MsgSubscription} 
                <div>
                    <p>_{MsgNotifyMeViaEmail}
                    $if null emails
                        <label>_{MsgEmailNotification}
                        <div>
                            <a .btn .btn-default href=@{AuthR registerR}>_{MsgNewEmail}
                    $else
                        <form .notification-form method=post enctype=#{notificationEnctype}>
                            ^{notificationWidget}                            
                            <button .btn .btn-default type=submit name=setting value=notification>_{MsgSave}
                    <p>-->
            <section .editor-setting>    
                <h2>_{MsgEditor}
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

        |]
        toWidget [lucius|
            li>a {
                padding:1em;
            }
            .name-form,.notification-form{
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
    currentTime<-liftIO $ getCurrentTime
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
        {-Just "avatar"-> do
            ((result,_), _) <- runFormPost $ avatarSettingForm user
            case result of
                FormSuccess res -> do 
                    _<-runDB $ update userId [UserAvatar =. _avatar res]
                    setMessageI $ MsgChangeSaved
                    redirect $ SettingsR
                _ -> notFound -}
        Just "notification"-> do
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

        

