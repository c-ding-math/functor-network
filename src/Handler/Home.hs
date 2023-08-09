{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
--import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
--import Text.Julius (RawJS (..))

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    maybeUserId<-maybeAuthId
    maybeFeatures<-runDB $ selectFirst [EntryInputTitle==."Features",EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]
    maybeScreenshots<-runDB $ selectFirst [EntryInputTitle==."Screenshots",EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]

    defaultLayout $ do  
        --aDomId <- newIdent
        setTitle $ toHtml $ "Welcome To " <> appName <> "!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    --maybeUserId<-maybeAuthId
    defaultLayout $ do
        --aDomId <- newIdent
        setTitle $ toHtml $ "Welcome To " <> appName <> "!"
        -- $(widgetFile "homepage")
