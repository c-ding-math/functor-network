{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Maintenance where

import Import
import Yesod.Form.Bootstrap3

maintenanceForm :: Form Maintenance
maintenanceForm = renderBootstrap3 BootstrapBasicForm $ Maintenance
    <$> lift (liftIO getCurrentTime)
    <*> areq intField "Duration (hours)" (Just 12)

getMaintenanceR :: Handler Html
getMaintenanceR = do
    (widget, enctype) <- generateFormPost maintenanceForm
    defaultLayout $ do
        setTitle "Maintenance"
        [whamlet|
            <h2>Site Maintenance
            <form method=post enctype=#{enctype}>
                ^{widget}
                <button .btn .btn-primary type=submit>Submit
        |]

postMaintenanceR :: Handler Html
postMaintenanceR = do
    ((result, widget), enctype) <- runFormPost maintenanceForm
    case result of
        FormSuccess maintenance -> do
            runDB $ insert_ maintenance
            setMessage "Maintenance scheduled"
            redirect MaintenanceR
        _ -> defaultLayout $ do
            setTitle "Please correct your entry form"
            [whamlet|
                <h2>Site Maintenance
                <form method=post enctype=#{enctype}>
                    ^{widget}
                    <button .btn .btn-primary type=submit>Submit
            |]
