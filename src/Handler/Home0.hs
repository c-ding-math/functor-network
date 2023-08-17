{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home0 where

import Import
--import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

getHome0R :: Handler Html
getHome0R = do
    maybeUserId<-maybeAuthId
    --maybeIntroduction<-runDB $ selectFirst [EntryInputTitle==."What is Functor Network",EntryType==.Page0,EntryStatus==.Draft] [Desc EntryInserted]
    maybeFeatures<-runDB $ selectFirst [EntryInputTitle==."Features",EntryType==.Page0,EntryStatus==.Draft] [Desc EntryInserted]
    maybeScreenshots<-runDB $ selectFirst [EntryInputTitle==."Screenshots",EntryType==.Page0,EntryStatus==.Draft] [Desc EntryInserted]

    defaultLayout $ do  
        --aDomId <- newIdent
        setTitle $ toHtml $ appName<>"—a blogging community for mathematicians"
        toWidgetHead
            [hamlet|
            <script type="application/ld+json">
                {
                    "@context" : "https://schema.org",
                    "@type" : "WebSite",
                    "name" : "#{appName}",
                    "url" : "@{Home0R}",
                }
            |]
        $(widgetFile "homepage")

postHome0R :: Handler Html
postHome0R = do
    --maybeUserId<-maybeAuthId
    defaultLayout $ do
        --aDomId <- newIdent
        setTitle $ toHtml $ "Welcome To " <> appName <> "!"
        -- $(widgetFile "homepage")
