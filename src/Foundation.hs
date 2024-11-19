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
import Control.Concurrent

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

-- modules for YesodAuthEmail instance
import           Yesod.Auth.Email
import           Network.Mail.Mime
import qualified Network.Mail.SMTP
import           Text.Shakespeare.Text         (stext)
import qualified Data.Text               
import qualified Data.Text.Lazy 
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Yesod.Auth.Message as Msg
import qualified Yesod.Auth.Extra  
import           Yesod.Form.Bootstrap3

import           Data.Either.Extra()
--import qualified Yesod.Auth.OAuth2 (getUserResponseJSON)
import qualified Yesod.Auth.OAuth2.Google
import qualified Yesod.Auth.OAuth2.ORCID
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
    | FooterLeft MenuItem
    | FooterRight MenuItem
    | FooterMiddle MenuItem

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
        1440  -- timeout in minutes
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
        mMaintenance<-runDB $ selectFirst [] [Desc MaintenanceFrom]
        --master <- getYesod
        mmsg <- getMessage

        --muser <- maybeAuthPair
        muid <- maybeAuthId
        mcurrentRoute <- getCurrentRoute
        mAuthorEntity<-routeUserEntity mcurrentRoute

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs
        (homeTitle, homeRoute)<- case mAuthorEntity of
            Just (Entity userId user) -> return (userName user, UserHomeR userId)
            Nothing -> return (appName, HomeR)

        -- Define the menu items of the header.
        let menuItems =
                case mAuthorEntity of
                    Just (Entity userId _) -> 
                        [ NavbarLeft $ MenuItem
                            { menuItemLabel = "Posts"
                            , menuItemRoute = UserEntriesR userId
                            , menuItemAccessCallback = True
                            }
                        , NavbarLeft $ MenuItem
                            { menuItemLabel = "Categories"
                            , menuItemRoute = CategoriesR userId
                            , menuItemAccessCallback = True
                            }
                        ]
                    _ -> 
                        [ NavbarLeft $ MenuItem
                            { menuItemLabel = "Posts"
                            , menuItemRoute = EntriesR
                            , menuItemAccessCallback = True
                            }
                        , NavbarLeft $ MenuItem
                            { menuItemLabel = "Members"
                            , menuItemRoute = UsersR
                            , menuItemAccessCallback = True
                            }
                        ]
                ++  case muid of 
                        Just uid ->
                            [ NavbarRight $ MenuItem
                                { menuItemLabel = "Write"
                                , menuItemRoute = NewUserEntryR
                                , menuItemAccessCallback = (entityKey <$> mAuthorEntity) == muid
                                }
                            ,NavbarRight $ MenuItem
                                { menuItemLabel = "Home"
                                , menuItemRoute = UserHomeR uid
                                , menuItemAccessCallback = (entityKey <$> mAuthorEntity) /= muid
                                }
                            , NavbarRight $ MenuItem
                                { menuItemLabel = "Settings"
                                , menuItemRoute = SettingsR
                                , menuItemAccessCallback = (entityKey <$> mAuthorEntity) == muid
                                }
                            , NavbarRight $ MenuItem
                                { menuItemLabel = "Logout"
                                , menuItemRoute = AuthR LogoutR
                                , menuItemAccessCallback = True
                                }
                            ]
                        _ ->
                            [ NavbarRight $ MenuItem
                                { menuItemLabel = "Login"
                                , menuItemRoute = AuthR LoginR
                                , menuItemAccessCallback = True
                                }
                            ]
                ++  case mcurrentRoute == Just (SettingsR) of
                        True -> 
                            [ FooterLeft $ MenuItem
                                { menuItemLabel = "Feedback"
                                , menuItemRoute = FeedbackR
                                , menuItemAccessCallback =  mcurrentRoute == Just (SettingsR)
                                }
                            , FooterRight $ MenuItem
                                { menuItemLabel = "Version 2024-11-19"
                                , menuItemRoute = PageR "Changelog"
                                , menuItemAccessCallback =  mcurrentRoute == Just (SettingsR)
                                }
                            ]
                        False -> 
                            [ FooterLeft $ MenuItem
                                { menuItemLabel = "About"
                                , menuItemRoute = PageR "About"
                                , menuItemAccessCallback = isNothing mAuthorEntity
                                }
                            , FooterLeft $ MenuItem
                                { menuItemLabel = "Network"
                                , menuItemRoute = HomeR
                                , menuItemAccessCallback = isJust mAuthorEntity
                                }
                            , FooterLeft $ MenuItem
                                { menuItemLabel = "Privacy"
                                , menuItemRoute = PageR "Privacy Policy"
                                , menuItemAccessCallback = True
                                }
                            , FooterLeft $ MenuItem
                                { menuItemLabel = "Feedback"
                                , menuItemRoute = FeedbackR
                                , menuItemAccessCallback = True
                                }
                            ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]
        let navbarMiddleMenuItems = [x | NavbarMiddle x <- menuItems]
        let footerLeftMenuItems = [x | FooterLeft x <- menuItems]
        let footerRightMenuItems = [x | FooterRight x <- menuItems]
        let footerMiddleMenuItems = [x | FooterMiddle x <- menuItems]      

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]
        let navbarMiddleFilteredMenuItems = [x | x <- navbarMiddleMenuItems, menuItemAccessCallback x]
        let footerLeftFilteredMenuItems = [x | x <- footerLeftMenuItems, menuItemAccessCallback x]
        let footerRightFilteredMenuItems = [x | x <- footerRightMenuItems, menuItemAccessCallback x]
        let footerMiddleFilteredMenuItems = [x | x <- footerMiddleMenuItems, menuItemAccessCallback x]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            --addStylesheetRemote "https://cdn.jsdelivr.net/npm/bootstrap@3.3.7/dist/css/bootstrap.min.css"
            --addStylesheetRemote "https://fonts.googleapis.com/css?family=Lato:400,700,400italic"
            addStylesheet $ StaticR css_bootstrap_min_css
            addStylesheet $ StaticR css_bootstrap_theme_css
            addScript $ StaticR js_bootstrap_min_js
            --addScript $ StaticR js_bootstrap_theme_js
            
            --addStylesheet $ StaticR css_entry_css
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
    isAuthorized HomeR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized UsersR _ = return Authorized
    isAuthorized (UserHomeR _) _ = return Authorized
    isAuthorized (UserEntriesR _) _ = return Authorized
    isAuthorized (CategoriesR _) _ = return Authorized
    --isAuthorized (UserPageR _ _) _ = return Authorized
    isAuthorized (UserEntryR _ _) _ = return Authorized
    isAuthorized (CommentsR _) _ = return Authorized
    isAuthorized (PageR _) _ = return Authorized
    isAuthorized (EditHelpR _) _ = return Authorized
    isAuthorized (ParserR _ _) _ = return Authorized
    isAuthorized (EditUserSubscriptionR _) _ = return Authorized
    isAuthorized (NewUserSubscriptionR _) _ = return Authorized
    isAuthorized (EditEntrySubscriptionR _) _ = return Authorized
    isAuthorized (NewEntrySubscriptionR _) _ = return Authorized
    isAuthorized EntriesR _ = return Authorized
    isAuthorized FeedbackR _ = return Authorized
    isAuthorized RedirectR _ = return Authorized

    -- Routes requiring authentication.
    --isAuthorized (EditUserPageR _) _ = isAuthenticated
    isAuthorized SettingsR _ = isAuthenticated
    isAuthorized EditUserAboutR _ = isAuthenticated
    isAuthorized AccountR _ = isAuthenticated
    isAuthorized FilesR _ = isAuthenticated
    --isAuthorized (ParserR _ _) _ = isAuthenticated
    isAuthorized (EditCommentR _) _ = isAuthenticated
    isAuthorized (EditFeedbackR _) _ = isAuthenticated
    isAuthorized NewUserEntryR _ = isAuthenticated
    isAuthorized NewCategoryR _ = isAuthenticated
    isAuthorized (TreeR _) _ = isAuthenticated
    isAuthorized (VoteR _) _ = isAuthenticated
    
    -- owner routes
    isAuthorized (EditUserEntryR entryId) _ = isAdmin entryId
    isAuthorized (EditCategoryR entryId) _ = isAdmin entryId
    isAuthorized (LoginSettingR x) _ = isAdmin x
    isAuthorized (EmailSettingR x) _ = isAdmin x
    isAuthorized (FileR x) _ = isAdmin x
    isAuthorized (SubscriptionsR x) _ = isAdmin x

    -- app administrator routes
    isAuthorized (EditEntryR _ ) _ = isAppAdministrator
    isAuthorized (EditPageR _ ) _ = isAppAdministrator
    isAuthorized PagesR _ = isAppAdministrator
    isAuthorized MaintenanceR _ = isAppAdministrator
    
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

    -- serve static files from a separate domain
    -- reference: https://www.yesodweb.com/book/yesod-typeclass
    urlParamRenderOverride site (StaticR s) _ =
        Just $ uncurry (joinPath site (appStaticRoot $ appSettings site)) $ renderRoute s
    urlParamRenderOverride _ _ _ = Nothing

    maximumContentLength _ (Just (FilesR)) = Just (256 * 1024 * 1024) -- 256MB
    maximumContentLength _ _               = Nothing -- default 2MB

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
                    _ -> "Anonymous User"
            return (siteName, Just HomeR)
        --UserPageR pathPiece _-> parentLink pathPiece
        UserEntriesR pathPiece -> parentLink pathPiece
        HomeR -> return ("Home", Nothing)
        AuthR _ -> return ("Home", Just HomeR)
        _ -> return ("home", Nothing)

      where
        parentLink :: Path -> Handler (Text, Maybe (Route App))
        parentLink pathPiece = do
            maybeUser <- runDB $ get pathPiece
            return $ case maybeUser of
                Just user -> (userName user, Just (UserHomeR pathPiece))
                _ -> ("Anonymous User", Just (UserHomeR pathPiece))
            

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
    loginDest _ = RedirectR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True
    
    --onLogin :: (MonadHandler m, master ~ HandlerSite m) => m () 
    onLogin = do
        muid <- maybeAuthId
        case muid of 
            Just uid -> do
                urlRenderParam <- getUrlRenderParams
                addMessage "success" $ [hamlet|
                    You are now logged in. #
                    <a .alert-link.pull-right href=@{UserEntriesR uid}>My Blog
                    <script>if (window.location.href == "@{UserEntriesR uid}"){document.querySelector(".alert-link").remove();}
                |] urlRenderParam 
            _ -> addMessage "success" "You are now logged in."

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        mCurrentUserId<-maybeAuthId
        let plugin= credsPlugin creds
            ident = credsIdent creds
            
        currentTime<-liftIO getCurrentTime
        msgRender<-getMessageRender
        
        case plugin of 
            
            x| x=="email" || x=="email-verify" -> do
                mEmail<- getBy $ UniqueEmail ident
                case mEmail of
                    Just (Entity _ email) | emailVerified email -> do
                        let muid=emailUserId email
                        case muid of 
                            Just uid->do
                                update uid [UserLogged=.currentTime]
                                return $ Authenticated uid
                            _ -> return $ UserError Msg.InvalidEmailAddress 
                    _ -> return $ UserError Msg.InvalidEmailAddress
                     
            _ -> do                        
                x <- getBy $ UniqueLogin plugin ident
                case x of 
                    Just (Entity _ login) -> do
                        case loginUserId login of 
                            Just uid-> do
                                update uid [UserLogged=.currentTime]
                                return $ Authenticated uid
                            _ -> return $ UserError Msg.InvalidLogin
                    Nothing -> do
                        case mCurrentUserId of
                            Just uid -> do
                                _<-insert $ Login {loginIdent=ident,loginPlugin=plugin,loginUserId=Just uid,loginToken=Nothing,loginVerified=True,loginInserted=currentTime}
                                return $ Authenticated uid
                            Nothing -> do 
                                --let uIdent = ident <> "@" <> plugin
                                uid<-insert $ User
                                    {userName=""
                                    ,userAvatar=Nothing
                                    ,userPassword=Nothing
                                    --,userIdent=uIdent
                                    ,userInserted=currentTime
                                    ,userLogged=currentTime
                                    --,userAvatar=Nothing
                                    ,userEmail=Nothing
                                    ,userDefaultFormat=Format "md"
                                    ,userDefaultPreamble=Just (Textarea "\\usepackage{amsmath, amssymb, amsfonts}\n\\newcommand{\\NN}{\\mathbb{N}}")
                                    ,userDefaultCitation=Nothing
                                    }
                                update uid [UserName=.(msgRender MsgUser <> " " <> (toPathPiece uid))]
                                {-case plugin of
                                    p| p `elem` ["google","orcid"] -> do
                                        case Yesod.Auth.Extra.pluginUserName <$> Yesod.Auth.OAuth2.getUserResponseJSON creds of
                                                Right n-> do
                                                    update uid [UserName=.n]
                                                    return ()
                                                _ -> return ()
                                        
                                    _ -> return ()-}
            
                                _<-insert $ Login {loginIdent=ident,loginPlugin=plugin,loginUserId=Just uid,loginToken=Nothing,loginVerified=True,loginInserted=currentTime}
                                return $ Authenticated uid
                                
    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = [authEmail] ++ [
                --Yesod.Auth.OAuth2.Google.oauth2GoogleScopedWidget googleButtonWidget ["openid", "email", "profile"] (appGoogleClientId (appSettings app)) (appGoogleClientSecret (appSettings app))
                Yesod.Auth.OAuth2.Google.oauth2Google (appGoogleClientId (appSettings app)) (appGoogleClientSecret (appSettings app))
                , Yesod.Auth.OAuth2.ORCID.oauth2ORCID (appORCIDClientId (appSettings app)) (appORCIDClientSecret (appSettings app))
            ] ++ extraAuthPlugins
        -- Enable authDummy login if enabled.
        where 
            extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

instance YesodAuthEmail App where
    type AuthEmailId App = EmailId

    afterPasswordRoute _ = RedirectR
    
    addUnverified email verkey = liftHandler $ runDB $ do
        maybeUserId<-maybeAuthId
        currentTime<-liftIO getCurrentTime
        --forwardedFor <- lookupHeader "X-Forwarded-For"
        request <- waiRequest
        let client = Just $ show request
        insert $ Email 
            {
                emailUserId=maybeUserId
                , emailAddress=email
                , emailVerkey= (Just verkey) 
                , emailVerified=False
                , emailInserted=currentTime
                , emailClient = client
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

        liftHandler $ sendAppEmail email $ AppEmail subject (text url) (html url)

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
        liftHandler $ sendAppEmail email $ AppEmail subject text html
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
                        newUserId<-insert $ User 
                                {userName=""
                                ,userAvatar=Nothing
                                ,userPassword=Nothing
                                --,userIdent=emailAddress email
                                ,userInserted=currentTime
                                ,userLogged=currentTime
                                --,userAvatar=Nothing
                                ,userEmail=Just $ emailAddress email
                                ,userDefaultFormat=Format "md"
                                ,userDefaultPreamble=Just (Textarea "\\usepackage{amsmath, amssymb, amsfonts}\n\\newcommand{\\NN}{\\mathbb{N}}")
                                ,userDefaultCitation=Nothing
                                }
                        update newUserId [UserName=.(msgRender MsgUser <> " " <> toPathPiece newUserId)]
                        
                        return newUserId
                        {-case newUser of
                            Left (Entity uid _) -> return uid -- existing user
                            Right uid -> do -- newly added user
                                let name = (msgRender MsgUser) <>" " <> (toPathPiece uid)
                                _<-update uid [UserName=.name]
                                return uid-}

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
                    setTitleI MsgNewEmail
                    [whamlet|
                        <p>_{Msg.EnterEmail}
                        <form #registerForm .form-inline method="post" action="@{toParentRoute registerR}" enctype=#{enctype}>
                                ^{widget}
                                <button .btn.btn-primary>_{MsgNewEmail}
                    |]
            Nothing -> 
                authLayout $ do
                    setTitleI Msg.RegisterLong
                    [whamlet|
                    <div .login-form-container>
                        <h3>_{Msg.Register}
                        <p>_{Msg.EnterEmail}
                        <form #registerForm method="post" action="@{toParentRoute registerR}" enctype=#{enctype}>
                                ^{widget}
                                
                        <label>
                            <input type=checkbox name=agree-required>
                            I agree to the <a href="@{PageR "Terms of Use"}">Terms</a> and <a href="@{PageR "Privacy Policy"}">Privacy Policy</a>                               
                        <button .btn.btn-primary disabled form=registerForm type=submit>_{Msg.Register}
                                
                    |]
                    loginStyle
                    loginWidget

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
            loginStyle=toWidget [lucius|
                .login-form-container{
                    margin: auto;    
                    width: 25em;    
                    padding: 0 2em;
                }
                .login-form-container input{
                    width:100%;
                    margin:0.5em 0;
                }   
                .login-form-container h3{
                    text-align:center;
                }
                .login-form-container button{
                    width:100%;
                    margin-bottom:1em;
                }

                .login-form-container input[type=checkbox]{
                    width: auto;
                    box-shadow: none;
                }
            |]
            loginWidget=toWidget [julius|
                //$(document).ready(function(){
                    $("input[name='agree-required']").change(function(){
                        if(this.checked){
                            $('button[type="submit"]').prop("disabled", false)
                        }else{
                            $('button[type="submit"]').prop("disabled", true)
                        }
                    });

                    $("#registerForm").submit(function(){
                        var agree = $("input[name='agree-required']").is(":checked");
                        if (!agree){
                            alert("Please agree to the Terms of Service and Privacy Policy.");
                            return false;
                        }
                    });
                //});
            |]

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
                        <button .btn .btn-primary>_{Msg.SendPasswordResetEmail}
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

    normalizeEmailAddress _ = id -- if it = Data.Text.toLower, then also add a javascript to the login form to lowercase the email address
    emailLoginHandler toParent = do
        (widget, enctype) <- generateFormPost loginForm

        [whamlet|
        <div .login-form-container>
            <form method="post" action="@{toParent loginR}" enctype=#{enctype}>
                <div id="emailLoginForm">
                    ^{widget}
                    <div>
                        <button .btn.btn-primary type=submit>
                            _{MsgLogIn}  
                        <a .btn.btn-default href="@{toParent registerR}">
                            _{MsgRegisterLong}
                        <a .btn.btn-default href="@{toParent forgotPasswordR}">
                            _{MsgForgotPassword}
                        <div .or>
                            <span>_{MsgOr}
                        <a .google.btn.btn-default href="#">       
                            <img src=@{StaticR icons_google_logo_svg}>
                            <span>_{MsgSignInWithGoogle}
                        <a .orcid.btn.btn-default href="#">
                            <img src=@{StaticR icons_orcid_logo_svg}>
                            <span>_{MsgSignInWithORCID}
        |]
        toWidget [julius|
            $("a.google").attr("href", $("a:contains('google')").attr("href"));
            $("a:contains('google')").remove();
            $("a.orcid").attr("href", $("a:contains('orcid')").attr("href"));
            $("a:contains('orcid')").remove();
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
            loginStyle=do
                {-toWidgetHead [hamlet|
                <link rel="preconnect" href="https://fonts.googleapis.com">
                <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
                <link href="https://fonts.googleapis.com/css2?family=Roboto:wght@500&display=swap" rel="stylesheet">
                |]-}
                toWidget [lucius|
                .login-form-container{
                    margin: auto;    
                    width: 25em;        
                }
                .login-form-container{
                    padding:2.5em 2.5em 0
                }
                .login-form-container input, .login-form-container button, .google-button{
                    width:100%;
                    margin-bottom:1em;
                }
                .login-form-container a {
                    display:inline-block;
                    text-align:center;
                    width:100%;
                    margin:0.5em 0;
                }
                .login-form-container h3{
                    text-align:center;
                }
                .google img, .orcid img{
                    height: 1.5em;
                    margin-right:0.5em;
                }
                |]

    -- | Response after sending a confirmation email.
    confirmationEmailSentResponse :: Text -> AuthHandler site TypedContent
    confirmationEmailSentResponse identifier = do
        mr <- getMessageRender
        selectRep $ do
            provideJsonMessage (mr msg)
            provideRep $ authLayout $ do
              setTitleI Msg.ConfirmationEmailSentTitle
              [whamlet|
                <p>A verification email has been sent to #{identifier}. Please click the link in the email to verify your account.
                <p>Receive no email? There are some steps you can take:
                <ul>
                    <li> Wait a minute.
                    <li> Make sure you typed your email address correctly.
                    <li> Add our notification email <code>noreply@functor.network</code> to your whitelist with your provider and try again.
                    <li> Send an email to <a href="mailto:feedback@functor.network">feedback@functor.network</a> and we will help you out.
              |]
      where
        msg = Msg.ConfirmationEmailSent identifier

    needOldPassword _ =return False

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
                        <div>
                            $if needOld
                                <div .form-group.required>
                                    <label>
                                        ^{fvLabel currentPasswordView}      
                                    ^{fvInput currentPasswordView}
                            <div .form-group.required>
                                <label>
                                    ^{fvLabel newPasswordView}                             
                                ^{fvInput newPasswordView}
                            <div .form-group.required>
                                <label>
                                    ^{fvLabel confirmPasswordView}
                                ^{fvInput confirmPasswordView}
                            <input .btn .btn-primary type=submit value=_{Msg.SetPassTitle}>
                    |]

            return (passwordFormRes, widget)
        currentPasswordSettings =
            FieldSettings {
                fsLabel = SomeMessage Msg.CurrentPassword,
                fsTooltip = Nothing,
                fsId = Just "currentPassword",
                fsName = Just "current",
                fsAttrs = [("autofocus", ""),("class","form-control")]
            }
        newPasswordSettings =
            FieldSettings {
                fsLabel = SomeMessage Msg.NewPass,
                fsTooltip = Nothing,
                fsId = Just "newPassword",
                fsName = Just "new",
                fsAttrs = [("autofocus", ""), (":not", ""), ("needOld:autofocus", ""), ("class","form-control")]
            }
        confirmPasswordSettings =
            FieldSettings {
                fsLabel = SomeMessage Msg.ConfirmPass,
                fsTooltip = Nothing,
                fsId = Just "confirmPassword",
                fsName = Just "confirm",
                fsAttrs = [("autofocus", ""), ("class","form-control")]
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
        |]
    
    checkPasswordSecurity :: AuthId site -> Text -> AuthHandler site (Either Text ())
    checkPasswordSecurity _ x
        | Data.Text.length x >= 8 = return $ Right ()
        | otherwise = return $ Left "Password must be at least 8 characters"

-- | send email 
data AppEmail = AppEmail
    { appEmailSubject :: Text
    , appEmailText :: Data.Text.Lazy.Text
    , appEmailHtml :: Html
    }
sendAppEmail ::  Text -> AppEmail -> Handler ()
sendAppEmail email appEmail= do
        master<-getYesod
        let systemEmailPassword=appEmailPassword $ appSettings master
            systemEmailHost=appEmailHost $ appSettings master
            systemEmailUser=appEmailUser $ appSettings master
            textMailPart = Part
                { partType = "text/plain; charset=utf-8"
                , partEncoding = None
                , partDisposition = DefaultDisposition
                , partContent = PartContent $ encodeUtf8 $ appEmailText appEmail
                , partHeaders = []
                }
            htmlMailPart = Part
                { partType = "text/html; charset=utf-8"
                , partEncoding = None
                , partDisposition = DefaultDisposition
                , partContent = PartContent $ renderHtml $ appEmailHtml appEmail
                , partHeaders = []
                }

        liftIO $ do
            _ <- forkIO $ do
                
                Network.Mail.SMTP.sendMailWithLoginTLS (unpack systemEmailHost) (unpack systemEmailUser) (unpack systemEmailPassword) (emptyMail $ Address Nothing systemEmailUser)
                    { mailTo = [Address Nothing email]
                    , mailHeaders =
                        [ ("Subject", appEmailSubject appEmail)
                        ]
                    , mailParts = [[textMailPart, htmlMailPart]]
                    }
                        
            return ()

-- | UTCTime to Date String
utcToDate :: UTCTime -> Text
utcToDate = Data.Text.pack . unwords . words . (formatTime defaultTimeLocale "%e %b %Y")

utcToDateTime :: UTCTime -> Text
utcToDateTime = Data.Text.pack . unwords . words . (formatTime defaultTimeLocale "%e %b %Y %H:%M")

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
    isAdministrator maybeUserId email | isJust maybeUserId && maybeUserId == emailUserId email && emailVerified email =  True
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

routeUserEntity :: Maybe (Route App) -> Handler (Maybe (Entity User))
routeUserEntity Nothing= return Nothing
routeUserEntity (Just route) 
    | "user" `member` routeAttrs route = case route of 
        UserHomeR userId -> returnEntityIfExist userId
        --UserPageR userId _ -> returnEntityIfExist userId
        CommentsR userId -> returnEntityIfExist userId
        UserEntriesR userId -> returnEntityIfExist userId
        CategoriesR userId -> returnEntityIfExist userId
        UserEntryR userId _ -> returnEntityIfExist userId
        _ -> do
            (userId, user) <- requireAuthPair
            return $ Just (Entity userId user)
    | otherwise = return Nothing

returnEntityIfExist :: (PersistEntityBackend val ~ SqlBackend, PersistEntity val) => Key val -> Handler (Maybe (Entity val))
returnEntityIfExist entityId = do
    mEntityValue<-runDB $ get entityId
    return $ case mEntityValue of
        Just entityValue -> Just (Entity entityId entityValue)
        _ -> Nothing

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

isAppAdministrator :: Handler AuthResult
isAppAdministrator = do
    currentUserId<-requireAuthId
    muid<-mAppAdministratorId
    if muid == Just currentUserId 
        then return Authorized
        else permissionDeniedI MsgPermissionDenied

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
