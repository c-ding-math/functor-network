{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}

module Foundation where

import Import.NoFoundation
import Data.Kind            (Type)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

-- modules for YesodAuthEmail instance
import           Yesod.Auth.Email
import           Network.Mail.Mime
import qualified Network.Mail.SMTP
import           Text.Shakespeare.Text         (stext)
import qualified Data.Text                     as TS
import qualified Data.Text.Lazy 
import qualified Data.Text.Lazy.Encoding
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Yesod.Auth.Message as Msg
import qualified Yesod.Auth.Extra  
import           Yesod.Form.Bootstrap3

--import           Database.Persist 
import           Data.Either.Extra()
import qualified Yesod.Auth.OAuth2 (getUserResponseJSON)
import qualified Yesod.Auth.OAuth2.Google
--import Yesod.Auth.OpenId    (authOpenId, IdentifierType (Claimed))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem
    | NavbarMiddle MenuItem

appName ::Text
appName = "Functor Network"
type Path = UserId  --For ease of modification

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()

mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | i18n
mkMessage "App" "messages" "en"

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: Type -> Type).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuthPair
        muid <- maybeAuthId
        mcurrentRoute <- getCurrentRoute

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs
        mUserRoutePath <- routeUser mcurrentRoute
        (homeTitle, homeRoute)<- case mUserRoutePath of
            Just userId -> do
                user <- runDB $ get404 userId
                return (userName user, UserHomeR userId)
            Nothing -> return (appName, HomeR)

        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = homeTitle
                    , menuItemRoute = homeRoute
                    , menuItemAccessCallback = True
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Login"
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                ] ++ case muid of 
                    Just uid | mUserRoutePath == muid->
                        [ NavbarMiddle $ MenuItem
                            { menuItemLabel = "Posts"
                            , menuItemRoute = EntriesR uid
                            , menuItemAccessCallback = True
                            }
                        , NavbarMiddle $ MenuItem
                            { menuItemLabel = "Comments"
                            , menuItemRoute = CommentsR uid
                            , menuItemAccessCallback = True
                            }
                        , NavbarMiddle $ MenuItem
                            { menuItemLabel = "Files"
                            , menuItemRoute = FilesR
                            , menuItemAccessCallback = True
                            }
                        , NavbarMiddle $ MenuItem
                            { menuItemLabel = "Settings"
                            , menuItemRoute = SettingsR
                            , menuItemAccessCallback = True
                            }
                        ]
                    Just uid | otherwise ->
                        [ NavbarMiddle $ MenuItem
                            { menuItemLabel = "My Homepage"
                            , menuItemRoute = UserHomeR uid
                            , menuItemAccessCallback = True
                            }
                        ]
                    _ -> []

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]
        let navbarMiddleMenuItems = [x | NavbarMiddle x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]
        let navbarMiddleFilteredMenuItems = [x | x <- navbarMiddleMenuItems, menuItemAccessCallback x]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_math_css
            addStylesheet $ StaticR css_bootstrap_css
                                    -- ^ generated from @Settings/StaticFiles.hs@
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized CommentR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized UsersR _ = return Authorized
    isAuthorized (UserHomeR _) _ = return Authorized
    isAuthorized (UserAboutR _) _ = return Authorized
    isAuthorized (EntriesR _) _ = return Authorized
    isAuthorized (EntryR _ _) _ = return Authorized
    isAuthorized (TagR _ _)_ = return Authorized
    isAuthorized (CommentsR _) _ = return Authorized
    isAuthorized (PageR _) _ = return Authorized

    -- Routes requiring authentication.
    isAuthorized (EditPageR _) _ = isAuthenticated
    isAuthorized SettingsR _ = isAuthenticated
    isAuthorized FilesR _ = isAuthenticated
    isAuthorized (ParserR _ _) _ = isAuthenticated
    isAuthorized (EditCommentR _) _ = isAuthenticated
    isAuthorized NewEntryR _ = isAuthenticated

    -- owner routes
    isAuthorized (EditEntryR entryId) _ = isAdmin entryId
    isAuthorized (LoginSettingR x) _ = isAdmin x
    isAuthorized (EmailSettingR x) _ = isAdmin x
    isAuthorized (FileR x) _ = isAdmin x
    
    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb
        :: Route App  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route App))
    
    breadcrumb route = case route of
        UserHomeR pathPiece -> do
            maybeUser <- runDB $ get pathPiece
            let siteName = case maybeUser of
                    Just user -> userName user
                    _ -> "Unknown"
            return (siteName, Just HomeR)
        UserAboutR pathPiece -> parentLink pathPiece
        EntriesR pathPiece -> parentLink pathPiece
        TagR pathPiece _-> parentLink pathPiece
        HomeR -> return ("Home", Nothing)
        AuthR _ -> return ("Home", Just HomeR)
        _ -> return ("home", Nothing)

      where
        parentLink :: Path -> Handler (Text, Maybe (Route App))
        parentLink pathPiece = do
            maybeUser <- runDB $ get pathPiece
            return $ case maybeUser of
                Just user -> (userName user, Just (UserHomeR pathPiece))
                _ -> ("Unknown", Just (UserHomeR pathPiece))
            

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = SettingsR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        mCurrentUserId<-maybeAuthId
        let plugin= credsPlugin creds
            ident = credsIdent creds
            
        currentTime<-liftIO getCurrentTime
        msgRender<-getMessageRender
        
        case plugin of 
            x|x=="email-verify" || x=="email" ->do
                mEmail<-getBy $ UniqueEmail ident
                case mEmail of
                    Just (Entity _ email) -> do
                        let muid=emailUserId email
                        case muid of 
                            Just uid->return $ Authenticated uid
                            _ -> return $ UserError Msg.InvalidEmailAddress 
                    _ -> return $ UserError Msg.InvalidEmailAddress
                     
            _ -> do                        
                x <- getBy $ UniqueLogin plugin ident
                case x of 
                    Just (Entity _ login) -> do
                        case loginUserId login of 
                            Just uid->return $ Authenticated uid
                            _ -> return $ UserError Msg.InvalidLogin
                    Nothing -> do
                        case mCurrentUserId of
                            Just uid -> do
                                _<-insert $ Login {loginIdent=ident,loginPlugin=plugin,loginUserId=Just uid,loginToken=Nothing,loginVerified=True,loginInserted=currentTime}
                                return $ Authenticated uid
                            Nothing -> do 
                                let uIdent = ident <> "@" <> plugin
                                eUser<-insertBy $ User
                                    {userIdent=uIdent
                                    ,userPassword=Nothing
                                    ,userName=""
                                    ,userInserted=currentTime
                                    --,userModified=currentTime
                                    --,userAvatar=Nothing
                                    ,userDefaultPreamble=Just (Textarea "\\usepackage{amsmath, amssymb, amsfonts}\n\\usepackage{tikz-cd}\n\\newcommand{\\NN}{\\mathbb{N}}")
                                    ,userDefaultCitation=Nothing
                                    }
                                case eUser of
                                    Left (Entity uid _) -> do
                                        _<-insert $ Login {loginIdent=ident,loginPlugin=plugin,loginUserId=Just uid,loginToken=Nothing,loginVerified=True,loginInserted=currentTime}
                                        return $ Authenticated uid
                                    Right uid -> do
                                        let name = case plugin of
                                                "google"->
                                                    case Yesod.Auth.Extra.googleUserName <$> Yesod.Auth.OAuth2.getUserResponseJSON creds of
                                                        Right n->n
                                                        _ -> (msgRender MsgUser) <>" " <> (toPathPiece uid)
                                                _ -> (msgRender MsgUser) <>" " <> (toPathPiece uid)
                                        _<-insert $ Login {loginIdent=ident,loginPlugin=plugin,loginUserId=Just uid,loginToken=Nothing,loginVerified=True,loginInserted=currentTime}
                                        _<-update uid [UserName=.name]
                                        return $ Authenticated uid

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = [authEmail] ++ [Yesod.Auth.OAuth2.Google.oauth2Google (appGoogleClientId (appSettings app)) (appGoogleClientSecret (appSettings app))] ++ extraAuthPlugins
        -- Enable authDummy login if enabled.
        where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

instance YesodAuthEmail App where
    type AuthEmailId App = EmailId

    afterPasswordRoute _ = HomeR

    addUnverified email verkey = liftHandler $ runDB $ do
        maybeUserId<-maybeAuthId -- update email if current user is logged in 
        currentTime<-liftIO getCurrentTime
        insert $ Email 
            {
                emailUserId=maybeUserId
                , emailAddress=email
                , emailVerkey= (Just verkey) 
                , emailVerified=False
                , emailInserted=currentTime
            }

    sendVerifyEmail email _ verurl = do
        hasSetPass<-liftHandler $ runDB $ do
            mEmail <- getBy $ UniqueEmail email
            case mEmail of   
                Just (Entity _ e) -> do                
                    let muid=emailUserId e
                    case muid of 
                        Nothing->return False
                        Just uid -> do
                            mUser<-get uid
                            return $ case mUser of
                                Just u | isJust (userPassword u)->True
                                _->False
                Nothing -> return False
        let url =case hasSetPass of
                True -> verurl<>"/has-set-pass"
                _->verurl

        liftHandler $ sendSystemEmail email subject (text url) (html url)

      where
        subject="Verify your email address"
        text url=   [stext|
                        Please confirm your email address by clicking on the link below.

                        #{url}

                        Thank you,
                        
                        #{appName}
                    |]
        html url=   [shamlet|
                        <p>Please confirm your email address by clicking on the link below.
                        <p>
                            <a href=#{url}>#{url}
                        <p>Thank you,
                        <p>#{appName}
                    |]
        --appName="Functor Network"::Text

    sendForgotPasswordEmail email _ verurl =  do
        liftHandler $ sendSystemEmail email subject text html
      where
        subject="Verify your email address"
        text =  [stext|
                    Please confirm your email address by clicking on the link below.

                    #{verurl}

                    Thank you,
                    
                    #{appName}
                |]
        html =  [shamlet|
                    <p>Please confirm your email address by clicking on the link below.
                    <p>
                        <a href=#{verurl}>#{verurl}
                    <p>Thank you,
                    <p>#{appName}
                |]
        --appName="Functor Network"::Text

    getVerifyKey = liftHandler . runDB . fmap (join . fmap emailVerkey) . get
    setVerifyKey lid key = liftHandler $ runDB $ update lid [EmailVerkey =. Just key]
    verifyAccount lid = liftHandler $ runDB $ do
        maybeEmail <- get lid
        currentTime<-liftIO getCurrentTime
        msgRender<-getMessageRender
        case maybeEmail of 
            Just email -> do
                let insertNewUser = do
                        newUser<-insertBy $ User 
                                {userIdent=emailAddress email
                                ,userPassword=Nothing
                                ,userName=""
                                ,userInserted=currentTime
                                --,userModified=currentTime
                                --,userAvatar=Nothing
                                ,userDefaultPreamble=Just (Textarea "\\usepackage{amsmath, amssymb, amsfonts}\n\\usepackage{tikz-cd}\n\\newcommand{\\NN}{\\mathbb{N}}")
                                ,userDefaultCitation=Nothing
                                }
                        case newUser of
                            Left (Entity uid _) -> return uid -- existing user
                            Right uid -> do -- newly added user
                                let name = (msgRender MsgUser) <>" " <> (toPathPiece uid)
                                _<-update uid [UserName=.name]
                                return uid

                uid <-  case emailUserId email of
                    Just uid-> do 
                        -- add another email to the existing account or reset the password
                        exisitence<-get uid
                        if isJust exisitence
                            then return uid
                            else insertNewUser
                    Nothing-> 
                        -- add a new user and a new login email
                        insertNewUser

                update lid $ 
                    [
                        EmailUserId =. Just uid
                        , EmailVerified =. True
                        , EmailVerkey =. Nothing
                    ]
                return $ Just uid
            Nothing -> return Nothing

    getPassword = liftHandler . runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = liftHandler . runDB $ update uid [UserPassword =. Just pass]
    getEmailCreds email = liftHandler $ runDB $ do     
        --liftIO $ putStrLn $ pack $ "show email"  
        mEmail <- getBy $ UniqueEmail email
        case mEmail of   
            Just (Entity eid e) -> do                
                let muid=emailUserId e
                status<-case muid of 
                    Nothing->return False
                    Just uid -> do
                        mUser<-get uid
                        return $ case mUser of
                            Just u | isJust (userPassword u)->True
                            _->False
                return $ Just EmailCreds
                    { emailCredsId = eid
                    , emailCredsAuthId = muid
                    , emailCredsStatus = status
                    , emailCredsVerkey = emailVerkey e
                    , emailCredsEmail = email
                    }
            _ -> return Nothing
    getEmail = liftHandler . runDB . fmap (fmap emailAddress) . get

    registerHandler = do
        (widget, enctype) <- generateFormPost registrationForm
        toParentRoute <- getRouteToParent
        muid<-maybeAuthId
        case muid of 
            Just _ ->
                authLayout $ do
                    setTitle "Add a new email"
                    [whamlet|
                        <p>_{Msg.EnterEmail}
                        <form .form-inline method="post" action="@{toParentRoute registerR}" enctype=#{enctype}>
                            <div id="registerForm">
                                ^{widget}
                                <button .btn .btn-default>Add a new email
                    |]
            Nothing -> 
                authLayout $ do
                    setTitleI Msg.RegisterLong
                    [whamlet|
                        <p>_{Msg.EnterEmail}
                        <form .form-inline method="post" action="@{toParentRoute registerR}" enctype=#{enctype}>
                            <div id="registerForm">
                                ^{widget}
                                <button .btn .btn-default>_{Msg.Register}
                    |]
        where
            
            registrationForm =  renderBootstrap3 BootstrapInlineForm $ Yesod.Auth.Extra.UserForm
                <$> areq emailField emailSettings Nothing    
            
            emailSettings = FieldSettings {
                    fsLabel = SomeMessage Msg.Email,
                    fsTooltip = Nothing,
                    fsId = Just "email",
                    fsName = Just "email",
                    fsAttrs = [("autofocus", ""), ("class", "form-control")]
                 }

    forgotPasswordHandler = do
        (widget, enctype) <- generateFormPost forgotPasswordForm
        toParent <- getRouteToParent
        authLayout $ do
            setTitleI Msg.PasswordResetTitle
            [whamlet|
                <p>_{MsgEnterPasswordResetEmail}
                <form .form-inline method=post action=@{toParent forgotPasswordR} enctype=#{enctype}>
                    <div id="forgotPasswordForm">
                        ^{widget}
                        <button .btn .btn-default>_{Msg.SendPasswordResetEmail}
            |]
        where
            forgotPasswordForm = renderBootstrap3 BootstrapInlineForm $ Yesod.Auth.Extra.ForgotPasswordForm
                <$> areq emailField emailSettings Nothing    

            emailSettings =
                FieldSettings {
                    fsLabel = SomeMessage Msg.ProvideIdentifier,
                    fsTooltip = Nothing,
                    fsId = Just "forgotPassword",
                    fsName = Just "email",
                    fsAttrs = [("autofocus", ""), ("class","form-control")]
                }

    emailLoginHandler toParent = do
        (widget, enctype) <- generateFormPost loginForm

        [whamlet|
        <div .login-form-container>
            <form method="post" action="@{toParent loginR}" enctype=#{enctype}>
                <div id="emailLoginForm">
                    ^{widget}
                    <div>
                        <button type=submit .btn .btn-primary>
                            _{Msg.LoginViaEmail}
                        
                        <a href="@{toParent registerR}">
                            _{Msg.RegisterLong}
                        <a href="@{toParent forgotPasswordR}">
                            _{MsgForgotPassword}
        |]
        loginStyle
        
        where
            --loginForm::Html -> MForm Handler (FormResult Yesod.Auth.Extra.UserLoginForm, Widget)
            loginForm = renderBootstrap3 BootstrapBasicForm $ Yesod.Auth.Extra.UserLoginForm
                    <$> areq emailField emailSettings Nothing  
                    <*> areq passwordField passwordSettings Nothing

            emailSettings = 
                FieldSettings {
                    fsLabel = SomeMessage Msg.Email,
                    fsTooltip = Nothing,
                    fsId = Just "email",
                    fsName = Just "email",
                    fsAttrs = [("autofocus", ""), ("class", "form-control")]
                }
            passwordSettings =
                FieldSettings {
                    fsLabel = SomeMessage Msg.Password,
                    fsTooltip = Nothing,
                    fsId = Just "password",
                    fsName = Just "password",
                    fsAttrs = [("class", "form-control")]
                }
            loginStyle=toWidget [lucius|
                .login-form-container, .login-form-container + a{
                    margin: auto;    
                    width: 25em;        
                }
                .login-form-container{
                    padding:2.5em 2.5em 0
                }
                .login-form-container input, .login-form-container button{
                    width:100%;
                    margin-bottom:1em;
                }
                .login-form-container a {
                    display:inline-block;
                    text-align:center;
                    width:100%;
                    margin:0.5em 0;
                }
                .login-form-container + a{
                    display:block;
                    text-align:center;
                    margin:0.5em auto;
                }
                .login-form-container h3{
                    text-align:center;
                }
            |]

    --needOldPassword _ =return False

    setPasswordHandler needOld = do
        messageRender <- getMessageRender
        toParent <- getRouteToParent
        selectRep $ do
            provideJsonMessage $ messageRender Msg.SetPass
            provideRep $ authLayout $ do
                (widget, enctype) <- generateFormPost setPasswordForm
                setTitleI Msg.SetPassTitle
                [whamlet|
                <div .login-form-container>
                    <h3>_{Msg.SetPass}
                    <form method="post" action="@{toParent setpassR}" enctype=#{enctype}>
                        ^{widget}
                |]
                loginStyle
      where
        setPasswordForm extra = do
            (currentPasswordRes, currentPasswordView) <- mreq passwordField currentPasswordSettings Nothing
            (newPasswordRes, newPasswordView) <- mreq passwordField newPasswordSettings Nothing
            (confirmPasswordRes, confirmPasswordView) <- mreq passwordField confirmPasswordSettings Nothing

            let passwordFormRes = Yesod.Auth.Extra.PasswordForm <$> currentPasswordRes <*> newPasswordRes <*> confirmPasswordRes
            let widget = do
                    [whamlet|
                        #{extra}
                        <table>
                            $if needOld
                                <tr>
                                    <th>
                                        ^{fvLabel currentPasswordView}
                                    <td>
                                        ^{fvInput currentPasswordView}
                            <tr>
                                <th>
                                    ^{fvLabel newPasswordView}
                                <td>
                                    ^{fvInput newPasswordView}
                            <tr>
                                <th>
                                    ^{fvLabel confirmPasswordView}
                                <td>
                                    ^{fvInput confirmPasswordView}
                            <tr>
                                <td colspan="2">
                                    <input .btn .btn-default type=submit value=_{Msg.SetPassTitle}>
                    |]

            return (passwordFormRes, widget)
        currentPasswordSettings =
            FieldSettings {
                fsLabel = SomeMessage Msg.CurrentPassword,
                fsTooltip = Nothing,
                fsId = Just "currentPassword",
                fsName = Just "current",
                fsAttrs = [("autofocus", "")]
            }
        newPasswordSettings =
            FieldSettings {
                fsLabel = SomeMessage Msg.NewPass,
                fsTooltip = Nothing,
                fsId = Just "newPassword",
                fsName = Just "new",
                fsAttrs = [("autofocus", ""), (":not", ""), ("needOld:autofocus", "")]
            }
        confirmPasswordSettings =
            FieldSettings {
                fsLabel = SomeMessage Msg.ConfirmPass,
                fsTooltip = Nothing,
                fsId = Just "confirmPassword",
                fsName = Just "confirm",
                fsAttrs = [("autofocus", "")]
            }
        loginStyle=toWidget [lucius|
            .login-form-container{
                margin: auto;    
                width: 25em;    
                padding: 0 2.5em;
            }
            .login-form-container input{
                width:100%;
                margin:0.5em 0;
            }   
            .login-form-container h3{
                text-align:center;
            }
            .login-form-container table{
                width:100%;
            }     
        |]
    
    checkPasswordSecurity :: AuthId site -> Text -> AuthHandler site (Either Text ())
    checkPasswordSecurity _ x
        | TS.length x >= 8 = return $ Right ()
        | otherwise = return $ Left "Password must be at least 8 characters"

-- | send email 
sendSystemEmail ::  Yesod.Auth.Email.Email ->Text->Data.Text.Lazy.Text->Html-> Handler ()
sendSystemEmail email subject text html= do
        master<-getYesod
        let systemEmailPassword=appEmailPassword $ appSettings master
            systemEmailHost=appEmailHost $ appSettings master
            systemEmailUser=appEmailUser $ appSettings master
        --liftIO $ putStrLn $ systemEmailHost ++ systemEmailPassword ++ systemEmailUser
        liftIO $ Network.Mail.SMTP.sendMailWithLoginTLS (unpack systemEmailHost) (unpack systemEmailUser) (unpack systemEmailPassword) (emptyMail $ Address Nothing systemEmailUser)
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", subject)
                ]
            , mailParts = [[textMailPart, htmlMailPart]]
            }

      where       
        textMailPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partDisposition = DefaultDisposition
            , partContent = PartContent $ Data.Text.Lazy.Encoding.encodeUtf8 text
            , partHeaders = []
            }
        htmlMailPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partDisposition = DefaultDisposition
            , partContent = PartContent $ renderHtml html
            , partHeaders = []
            }

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

-- | Access function to determine if a user is administrator
class PersistEntity a => Administrator a where
    isAdministrator::Maybe UserId ->a->Bool

instance Administrator Login where
    isAdministrator maybeUserId login | isJust maybeUserId && maybeUserId == loginUserId login =  True
    isAdministrator _ _= False
instance Administrator Import.NoFoundation.Email where
    isAdministrator maybeUserId email | isJust maybeUserId && maybeUserId == emailUserId email =  True
    isAdministrator _ _= False
instance Administrator File where
    isAdministrator maybeUserId file | maybeUserId == Just (fileUserId file) =  True
    isAdministrator _ _= False
instance Administrator Entry where
    isAdministrator maybeUserId entry | maybeUserId == Just (entryUserId entry) =  True
    isAdministrator _ _= False

isAdmin:: (Administrator a, PersistEntityBackend a ~ SqlBackend)=>Key a->Handler AuthResult
isAdmin key = do
    x<-runDB $ get404 key
    uid<-requireAuthId
    if isAdministrator (Just uid) x
        then return Authorized
        else permissionDeniedI MsgPermissionDenied

isSiteAdmin::Path->Handler AuthResult
isSiteAdmin piece=do
    muid<-maybeAuthId
    if muid == Just piece
        then return Authorized
        else permissionDeniedI MsgPermissionDenied

routeUser :: Maybe (Route App) -> Handler (Maybe UserId)
routeUser Nothing= return Nothing
routeUser (Just route) 
    | "user" `member` routeAttrs route = case route of 
        UserHomeR userId -> return $ Just userId
        UserAboutR userId -> return $ Just userId
        CommentsR userId -> return $ Just userId
        EntriesR userId -> return $ Just userId
        EntryR userId _ -> return $ Just userId
        TagR userId _ -> return $ Just userId
        _ -> do
            uid<-requireAuthId
            return $ Just uid
    | otherwise = return Nothing

-- | get the app administrator id
mAppAdministratorId :: Handler (Maybe UserId)
mAppAdministratorId = do
    
    master<-getYesod
    mAppAdministatorEmail <- runDB $ getBy $ UniqueEmail $ appAdministratorEmail $ appSettings master
    return $ case mAppAdministatorEmail of
            Just (Entity _ appAdministatorEmail) -> emailUserId appAdministatorEmail 
            Nothing -> Nothing
    
requireAppAdministratorId :: Handler UserId
requireAppAdministratorId = do
    muid<-mAppAdministratorId
    case muid of
        Just uid -> return uid
        Nothing -> notFound

{-requireAdminId :: Path -> Handler (UserId, SiteId)
requireAdminId piece = do
    uid<-requireAuthId   
    runDB $ do 
        mSite<- getBy $ UniqueSite piece
        case mSite of
            Nothing -> notFound
            Just (Entity sid _) -> do
                roles <- selectList [RoleType>=.Administrator,RoleUserId==.uid,RoleSiteId==.sid] []
                if (null roles)     
                    then permissionDeniedI MsgPermissionDenied
                    else return (uid,sid)

requireAdmin :: Path -> Handler (User, Site)
requireAdmin piece = do
    (uid, u)<-requireAuthPair
    runDB $ do 
        mSite<- getBy $ UniqueSite piece
        case mSite of
            Nothing -> notFound
            Just (Entity sid s) -> do
                roles <- selectList [RoleType>=.Administrator,RoleUserId==.uid,RoleSiteId==.sid] []
                if (null roles)     
                    then permissionDeniedI MsgPermissionDenied
                    else return (u,s)
-}
instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding