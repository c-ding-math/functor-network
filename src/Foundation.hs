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
        10080  -- timeout in minutes
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
                                { menuItemLabel = "<span title='Write' class='hidden-xs'><svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' fill='currentColor' class='bi bi-pencil-square' viewBox='0 0 16 16'><path d='M15.502 1.94a.5.5 0 0 1 0 .706L14.459 3.69l-2-2L13.502.646a.5.5 0 0 1 .707 0l1.293 1.293zm-1.75 2.456-2-2L4.939 9.21a.5.5 0 0 0-.121.196l-.805 2.414a.25.25 0 0 0 .316.316l2.414-.805a.5.5 0 0 0 .196-.12l6.813-6.814z'/><path fill-rule='evenodd' d='M1 13.5A1.5 1.5 0 0 0 2.5 15h11a1.5 1.5 0 0 0 1.5-1.5v-6a.5.5 0 0 0-1 0v6a.5.5 0 0 1-.5.5h-11a.5.5 0 0 1-.5-.5v-11a.5.5 0 0 1 .5-.5H9a.5.5 0 0 0 0-1H2.5A1.5 1.5 0 0 0 1 2.5z'/></svg></span> <span class='hidden-sm'>Write</span>"
                                , menuItemRoute = NewUserEntryR
                                , menuItemAccessCallback = (entityKey <$> mAuthorEntity) == muid
                                }
                            , NavbarRight $ MenuItem
                                { menuItemLabel = "<span title='Home' class='hidden-xs'><svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' fill='currentColor' class='bi bi-house' viewBox='0 0 16 16'><path d='M8.707 1.5a1 1 0 0 0-1.414 0L.646 8.146a.5.5 0 0 0 .708.708L2 8.207V13.5A1.5 1.5 0 0 0 3.5 15h9a1.5 1.5 0 0 0 1.5-1.5V8.207l.646.647a.5.5 0 0 0 .708-.708L13 5.793V2.5a.5.5 0 0 0-.5-.5h-1a.5.5 0 0 0-.5.5v1.293zM13 7.207V13.5a.5.5 0 0 1-.5.5h-9a.5.5 0 0 1-.5-.5V7.207l5-5z'/></svg></span> <span class='hidden-sm'>Home</span>"
                                , menuItemRoute = UserHomeR uid
                                , menuItemAccessCallback = (entityKey <$> mAuthorEntity) /= muid
                                }
                            , NavbarRight $ MenuItem
                                { menuItemLabel = "<span title='Settings' class='hidden-xs'><svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' fill='currentColor' class='bi bi-gear' viewBox='0 0 16 16'><path d='M8 4.754a3.246 3.246 0 1 0 0 6.492 3.246 3.246 0 0 0 0-6.492M5.754 8a2.246 2.246 0 1 1 4.492 0 2.246 2.246 0 0 1-4.492 0'/><path d='M9.796 1.343c-.527-1.79-3.065-1.79-3.592 0l-.094.319a.873.873 0 0 1-1.255.52l-.292-.16c-1.64-.892-3.433.902-2.54 2.541l.159.292a.873.873 0 0 1-.52 1.255l-.319.094c-1.79.527-1.79 3.065 0 3.592l.319.094a.873.873 0 0 1 .52 1.255l-.16.292c-.892 1.64.901 3.434 2.541 2.54l.292-.159a.873.873 0 0 1 1.255.52l.094.319c.527 1.79 3.065 1.79 3.592 0l.094-.319a.873.873 0 0 1 1.255-.52l.292.16c1.64.893 3.434-.902 2.54-2.541l-.159-.292a.873.873 0 0 1 .52-1.255l.319-.094c1.79-.527 1.79-3.065 0-3.592l-.319-.094a.873.873 0 0 1-.52-1.255l.16-.292c.893-1.64-.902-3.433-2.541-2.54l-.292.159a.873.873 0 0 1-1.255-.52zm-2.633.283c.246-.835 1.428-.835 1.674 0l.094.319a1.873 1.873 0 0 0 2.693 1.115l.291-.16c.764-.415 1.6.42 1.184 1.185l-.159.292a1.873 1.873 0 0 0 1.116 2.692l.318.094c.835.246.835 1.428 0 1.674l-.319.094a1.873 1.873 0 0 0-1.115 2.693l.16.291c.415.764-.42 1.6-1.185 1.184l-.291-.159a1.873 1.873 0 0 0-2.693 1.116l-.094.318c-.246.835-1.428.835-1.674 0l-.094-.319a1.873 1.873 0 0 0-2.692-1.115l-.292.16c-.764.415-1.6-.42-1.184-1.185l.159-.291A1.873 1.873 0 0 0 1.945 8.93l-.319-.094c-.835-.246-.835-1.428 0-1.674l.319-.094A1.873 1.873 0 0 0 3.06 4.377l-.16-.292c-.415-.764.42-1.6 1.185-1.184l.292.159a1.873 1.873 0 0 0 2.692-1.115z'/></svg></span> <span class='hidden-sm'>Settings</span>"
                                , menuItemRoute = SettingsR
                                , menuItemAccessCallback = (entityKey <$> mAuthorEntity) == muid
                                }
                            , NavbarRight $ MenuItem
                                { menuItemLabel = "<span title='Logout' class='hidden-xs'><svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' fill='currentColor' class='bi bi-box-arrow-left' viewBox='0 0 16 16'><path fill-rule='evenodd' d='M6 12.5a.5.5 0 0 0 .5.5h8a.5.5 0 0 0 .5-.5v-9a.5.5 0 0 0-.5-.5h-8a.5.5 0 0 0-.5.5v2a.5.5 0 0 1-1 0v-2A1.5 1.5 0 0 1 6.5 2h8A1.5 1.5 0 0 1 16 3.5v9a1.5 1.5 0 0 1-1.5 1.5h-8A1.5 1.5 0 0 1 5 12.5v-2a.5.5 0 0 1 1 0z'/><path fill-rule='evenodd' d='M.146 8.354a.5.5 0 0 1 0-.708l3-3a.5.5 0 1 1 .708.708L1.707 7.5H10.5a.5.5 0 0 1 0 1H1.707l2.147 2.146a.5.5 0 0 1-.708.708z'/></svg></span> <span class='hidden-sm'>Logout</span>"
                                , menuItemRoute = AuthR LogoutR
                                , menuItemAccessCallback = True
                                }
                            ]
                        _ ->
                            [ NavbarRight $ MenuItem
                                { menuItemLabel = "<span title='Login'><svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' fill='currentColor' class='bi bi-box-arrow-in-right' viewBox='0 0 16 16'><path fill-rule='evenodd' d='M6 3.5a.5.5 0 0 1 .5-.5h8a.5.5 0 0 1 .5.5v9a.5.5 0 0 1-.5.5h-8a.5.5 0 0 1-.5-.5v-2a.5.5 0 0 0-1 0v2A1.5 1.5 0 0 0 6.5 14h8a1.5 1.5 0 0 0 1.5-1.5v-9A1.5 1.5 0 0 0 14.5 2h-8A1.5 1.5 0 0 0 5 3.5v2a.5.5 0 0 0 1 0z'/><path fill-rule='evenodd' d='M11.854 8.354a.5.5 0 0 0 0-.708l-3-3a.5.5 0 1 0-.708.708L10.293 7.5H1.5a.5.5 0 0 0 0 1h8.793l-2.147 2.146a.5.5 0 0 0 .708.708z'/></svg></span> <span class='hidden-sm'>Login</span>"
                                , menuItemRoute = AuthR LoginR
                                , menuItemAccessCallback = True
                                }
                            ]
                ++  
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
                                { menuItemLabel = "Tools"  
                                , menuItemRoute = PageR "Tools"
                                , menuItemAccessCallback = isNothing mAuthorEntity
                                }              
                            , FooterLeft $ MenuItem
                                { menuItemLabel = "Feedback"
                                , menuItemRoute = FeedbackR
                                , menuItemAccessCallback = True
                                }
                            , FooterRight $ MenuItem
                                { menuItemLabel = "<span><svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' fill='currentColor' class='bi bi-mastodon' viewBox='0 0 16 16'><path d='M11.19 12.195c2.016-.24 3.77-1.475 3.99-2.603.348-1.778.32-4.339.32-4.339 0-3.47-2.286-4.488-2.286-4.488C12.062.238 10.083.017 8.027 0h-.05C5.92.017 3.942.238 2.79.765c0 0-2.285 1.017-2.285 4.488l-.002.662c-.004.64-.007 1.35.011 2.091.083 3.394.626 6.74 3.78 7.57 1.454.383 2.703.463 3.709.408 1.823-.1 2.847-.647 2.847-.647l-.06-1.317s-1.303.41-2.767.36c-1.45-.05-2.98-.156-3.215-1.928a4 4 0 0 1-.033-.496s1.424.346 3.228.428c1.103.05 2.137-.064 3.188-.189zm1.613-2.47H11.13v-4.08c0-.859-.364-1.295-1.091-1.295-.804 0-1.207.517-1.207 1.541v2.233H7.168V5.89c0-1.024-.403-1.541-1.207-1.541-.727 0-1.091.436-1.091 1.296v4.079H3.197V5.522q0-1.288.66-2.046c.456-.505 1.052-.764 1.793-.764.856 0 1.504.328 1.933.983L8 4.39l.417-.695c.429-.655 1.077-.983 1.934-.983.74 0 1.336.259 1.791.764q.662.757.661 2.046z'/></svg></span>"
                                , menuItemRoute = PageR "https://mathstodon.xyz/@dingc"
                                , menuItemAccessCallback = isNothing mAuthorEntity
                                }
                            , FooterRight $ MenuItem
                                { menuItemLabel = "<span><svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' fill='currentColor' class='bi bi-twitter-x' viewBox='0 0 16 16'><path d='M12.6.75h2.454l-5.36 6.142L16 15.25h-4.937l-3.867-5.07-4.425 5.07H.316l5.733-6.57L0 .75h5.063l3.495 4.633L12.601.75Zm-.86 13.028h1.36L4.323 2.145H2.865z'/></svg></span>"
                                , menuItemRoute = PageR "https://x.com/functor_network"
                                , menuItemAccessCallback = isNothing mAuthorEntity
                                }
                            , FooterRight $ MenuItem
                                { menuItemLabel = "<span><svg xmlns='http://www.w3.org/2000/svg' preserveAspectRatio='none' width='16' height='16' viewBox='15 0 210 194' fill='none'><mask id='mask0_1_219' style='mask-type:luminance' maskUnits='userSpaceOnUse' x='-1' y='0' width='242' height='194'><path d='M240.469 0.958984H-0.00585938V193.918H240.469V0.958984Z' fill='white'/></mask><g mask='url(#mask0_1_219)'><path d='M96.1344 193.911C61.1312 193.911 32.6597 178.256 15.9721 149.829C1.19788 124.912 -0.00585938 97.9229 -0.00585938 67.7662C-0.00585938 49.8876 5.37293 34.3215 15.5413 22.7466C24.8861 12.1157 38.1271 5.22907 52.8317 3.35378C70.2858 1.14271 91.9848 0.958984 114.545 0.958984C151.259 0.958984 161.63 1.4088 176.075 2.85328C195.29 4.76026 211.458 11.932 222.824 23.5955C234.368 35.4428 240.469 51.2624 240.469 69.3627V72.9994C240.469 103.885 219.821 129.733 191.046 136.759C188.898 141.827 186.237 146.871 183.089 151.837L183.006 151.964C172.869 167.632 149.042 193.918 103.401 193.918H96.1281L96.1344 193.911Z' fill='white'/><path d='M174.568 17.9772C160.927 16.6151 151.38 16.1589 114.552 16.1589C90.908 16.1589 70.9008 16.387 54.7644 18.4334C33.3949 21.164 15.2058 37.5285 15.2058 67.7674C15.2058 98.0066 16.796 121.422 29.0741 142.107C42.9425 165.751 66.1302 178.707 96.1412 178.707H103.414C140.242 178.707 160.25 159.156 170.253 143.698C174.574 136.874 177.754 130.058 179.801 123.234C205.947 120.96 225.27 99.3624 225.27 72.9941V69.3577C225.27 40.9432 206.631 21.164 174.574 17.9772H174.568Z' fill='white'/><path d='M15.1975 67.7674C15.1975 37.5285 33.3866 21.164 54.7559 18.4334C70.8987 16.387 90.906 16.1589 114.544 16.1589C151.372 16.1589 160.919 16.6151 174.559 17.9772C206.617 21.1576 225.255 40.937 225.255 69.3577V72.9941C225.255 99.3687 205.932 120.966 179.786 123.234C177.74 130.058 174.559 136.874 170.238 143.698C160.235 159.156 140.228 178.707 103.4 178.707H96.1264C66.1155 178.707 42.9277 165.751 29.0595 142.107C16.7814 121.422 15.1912 98.4563 15.1912 67.7674' fill='currentColor'/><path d='M32.2469 67.9899C32.2469 97.3168 34.0654 116.184 43.6127 133.689C54.5225 153.924 74.3018 161.653 96.8117 161.653H103.857C133.411 161.653 147.736 147.329 155.693 134.829C159.558 128.462 162.966 121.417 164.784 112.547L166.147 106.864H174.332C192.521 106.864 208.208 92.09 208.208 73.2166V69.8082C208.208 48.6669 195.024 37.5228 172.058 34.7987C159.102 33.6646 151.372 33.2084 114.538 33.2084C89.7602 33.2084 72.0272 33.4364 58.6152 35.4828C39.7483 38.2134 32.2407 48.8951 32.2407 67.9899' fill='white'/><path d='M166.158 83.6801C166.158 86.4107 168.204 88.4572 171.841 88.4572C183.435 88.4572 189.802 81.8619 189.802 70.9523C189.802 60.0427 183.435 53.2195 171.841 53.2195C168.204 53.2195 166.158 55.2657 166.158 57.9963V83.6866V83.6801Z' fill='currentColor'/><path d='M54.5321 82.3198C54.5321 95.732 62.0332 107.326 71.5807 116.424C77.9478 122.562 87.9515 128.93 94.7685 133.022C96.8147 134.157 98.8611 134.841 101.136 134.841C103.866 134.841 106.134 134.157 107.959 133.022C114.782 128.93 124.779 122.562 130.919 116.424C140.694 107.332 148.195 95.7383 148.195 82.3198C148.195 67.7673 137.286 54.8115 121.599 54.8115C112.28 54.8115 105.912 59.5882 101.136 66.1772C96.8147 59.582 90.2259 54.8115 80.9001 54.8115C64.9855 54.8115 54.5256 67.7673 54.5256 82.3198' fill='currentColor'/></g></svg></span>"
                                , menuItemRoute = PageR "https://ko-fi.com/functor_network"
                                , menuItemAccessCallback = isNothing mAuthorEntity
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
            addScript $ StaticR js_bootstrap_theme_js
            
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
    isAuthorized EntriesR _ = return Authorized
    isAuthorized FeedbackR _ = return Authorized
    isAuthorized (UserHomeR _) _ = return Authorized
    isAuthorized (UserEntriesR _) _ = return Authorized
    isAuthorized (CategoriesR _) _ = return Authorized
    --isAuthorized (UserPageR _ _) _ = return Authorized
    isAuthorized (UserEntryR _ _) _ = return Authorized
    isAuthorized (CommentsR _) _ = return Authorized
    isAuthorized (PageR _) _ = return Authorized
    isAuthorized (EditHelpR _) _ = return Authorized
    isAuthorized (ParseR _ _) _ = return Authorized
    isAuthorized (EditUserSubscriptionR _) _ = return Authorized
    isAuthorized (NewUserSubscriptionR _) _ = return Authorized
    isAuthorized (EditEntrySubscriptionR _) _ = return Authorized
    isAuthorized (NewEntrySubscriptionR _) _ = return Authorized
    isAuthorized (ToolR _) _ = return Authorized
    isAuthorized (UserFeedR _) _ = return Authorized
    isAuthorized SitemapR _ = return Authorized
    isAuthorized (DownloadR _ _) _ = return Authorized
    isAuthorized (LikeR _) _ = return Authorized
    --isAuthorized RedirectR _ = return Authorized

    -- Routes requiring authentication.
    --isAuthorized (EditUserPageR _) _ = isAuthenticated
    isAuthorized SettingsR _ = isAuthenticated
    isAuthorized EditUserAboutR _ = isAuthenticated
    isAuthorized AccountR _ = isAuthenticated
    isAuthorized FilesR _ = isAuthenticated
    isAuthorized (EditCommentR _) _ = isAuthenticated
    isAuthorized (EditFeedbackR _) _ = isAuthenticated
    isAuthorized NewUserEntryR _ = isAuthenticated
    isAuthorized NewCategoryR _ = isAuthenticated
    isAuthorized (TreeR _) _ = isAuthenticated
    isAuthorized (VoteR _) _ = isAuthenticated
    isAuthorized DataR _ = isAuthenticated
    
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
    isAuthorized (MaintainEntryR _ ) _ = isAppAdministrator
    isAuthorized SlugR _ = isAppAdministrator
    
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
    loginDest _ = HomeR
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
                |] urlRenderParam 
            _ -> addMessage "success" "You are now logged in."

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ do 
      mCurrentUserId<-maybeAuthId
      currentTime<-liftIO getCurrentTime
      msgRender<-getMessageRender
      runDB $ do
        let plugin= credsPlugin creds
            ident = credsIdent creds
        
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

    afterPasswordRoute _ = SettingsR
    
    addUnverified email verkey = liftHandler $ runDB $ do
        maybeUserId<-maybeAuthId
        currentTime<-liftIO getCurrentTime
        --forwardedFor <- lookupHeader "X-Forwarded-For"
        --request <- waiRequest
        --let client = Just $ show request
        insert $ Email 
            {
                emailUserId=maybeUserId
                , emailAddress=email
                , emailVerkey= (Just verkey) 
                , emailVerified=False
                , emailInserted=currentTime
                , emailClient = Nothing
            }

    sendVerifyEmail email verkey _ = liftHandler $ sendAppEmail email $ AppEmail subject text html
      where
        --workaround: It is the registration workflow if the key begins with "r"
        key = "r" <> verkey
        subject="Verify your email address"
        text =  [stext|
                    Please confirm your email address with the verification key below.

                    #{key}

                    Thank you,
                    
                    #{appName}
                |]
        html =  [shamlet|
                    <p>Please confirm your email address with the verification key below.
                    <div style="font-size:1.5em; font-weight:bold;">
                        #{key}
                    <p>Thank you,
                    <p>#{appName}
                |]

    sendForgotPasswordEmail email verkey _ = liftHandler $ sendAppEmail email $ AppEmail subject text html
      where
        --workaround: It is the forgot password workflow if the key begins with "f"
        key = "f" <> verkey
        subject="Verify your email address"
        text =  [stext|
                    Please confirm your email address with the verification key below.

                    #{key}

                    Thank you,
                    
                    #{appName}
                |]
        html =  [shamlet|
                    <p>Please confirm your email address with the verification key below.
                    <div style="font-size:1.5em; font-weight:bold;">
                        #{key}
                    <p>Thank you,
                    <p>#{appName}
                |]

    getVerifyKey = liftHandler . runDB . fmap (join . fmap emailVerkey) . get
    setVerifyKey lid key = liftHandler $ runDB $ update lid [EmailVerkey =. Just key]
    verifyAccount lid = liftHandler $ do
      currentTime<-liftIO getCurrentTime
      msgRender<-getMessageRender
      runDB $ do
        maybeEmail <- get lid
        
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
    confirmationEmailSentResponse identifier = do
        toParent <- getRouteToParent
        let toBeReplaced = "TOBEREPLACED"::Text
        verificationRoute <- liftHandler $ runDB $ do
            mEmail <- getBy $ UniqueEmail identifier
            case mEmail of   
                Just (Entity eid e) -> do                
                    hasSetPass <- case emailUserId e of 
                        Nothing->return False
                        Just uid -> do
                            mUser<-get uid
                            return $ case mUser of
                                Just u -> isJust (userPassword u)
                                _->False
                    return $ toParent $ verifyR (toPathPiece eid) toBeReplaced hasSetPass
                Nothing -> notFound
        
        mr <- getMessageRender
        selectRep $ do
            provideJsonMessage (mr (Msg.ConfirmationEmailSent identifier))
            provideRep $ authLayout $ do
              setTitleI Msg.ConfirmationEmailSentTitle
              [whamlet|
                <p>_{Msg.ConfirmationEmailSent identifier} _{MsgEnterVerificationKey}
                <div class="form-inline">
                    <div .form-group.required>
                        <label class="sr-only">_{MsgVerificationKey}
                        <input .form-control type=text name=key placeholder=_{MsgVerificationKey}>
                    <button #confirm-button .btn .btn-primary type=button>_{MsgConfirm}
                <hr>
                <p>Receive no email? There are some steps you can take:
                <ul>
                    <li> Wait a minute.
                    <li> Make sure you typed your email address correctly.
                    <li> Add our notification email <code>notification@functor.network</code> to your whitelist with your provider and try again.
                    <li> <a href=@{FeedbackR}>Feedback here</a>. We will help you out.
              |]
              toWidget [julius|
                $("input[name=key]").focus();
                $("#confirm-button").on("click", function(){
                    let key = $("input[name=key]").val();
                    let isForgotPasswordWorkflow;
                    if (key.charAt(0) == "f"){
                        isForgotPasswordWorkflow = true;
                    } else {
                        isForgotPasswordWorkflow = false;
                    }
                    let toBeReplaced = #{toBeReplaced};
                        verificationRoute = "@{verificationRoute}";
                        windowLocation = verificationRoute.replace(toBeReplaced, key.substr(1));
                    if (isForgotPasswordWorkflow){
                        window.location = windowLocation.replace("/has-set-pass","");
                    } else {
                        window.location.href = windowLocation;
                    }
                });
              |]

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
