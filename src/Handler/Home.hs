{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
--import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
--import Text.Julius (RawJS (..))

getHomeR :: Handler Html
getHomeR = do
    maybeUserId<-maybeAuthId
    --maybeIntroduction<-runDB $ selectFirst [EntryTitle==."What is Functor Network",EntryType==.Page0,EntryStatus==.Draft] [Desc EntryInserted]
    maybeFeatures<-runDB $ selectFirst [EntryTitle==."Features",EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]
    maybeScreenshots<-runDB $ selectFirst [EntryTitle==."Screenshots",EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]

    defaultLayout $ do  
        --aDomId <- newIdent
        setTitle $ toHtml $ appName
        setDescriptionIdemp $ "Functor Network is a blog platform for mathematicians. It is featured with effortless typesetting, dual latex and markdown support, latex package support, automated numbering and referencing, clean and focused design, and more."
        toWidgetHead
            [hamlet|
                <link rel="me" href="https://mathstodon.xyz/@dingc">
            |]
        toWidgetHead
            [hamlet|
            <script type="application/ld+json">
                {
                    "@context" : "https://schema.org",
                    "@type" : "WebSite",
                    "name" : "#{appName}",
                    "url" : "@{HomeR}"
                }
            |]
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    --maybeUserId<-maybeAuthId
    defaultLayout $ do
        --aDomId <- newIdent
        setTitle $ toHtml $ "Welcome To " <> appName <> "!"
        -- $(widgetFile "homepage")
