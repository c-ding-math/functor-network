{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.EditEntrySubscription where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Handler.Subscriptions (getSubscriptions)

getEditEntrySubscriptionR :: EntrySubscriptionId -> Handler Html
getEditEntrySubscriptionR subscriptionId = do
    subscription <- runDB $ get404 subscriptionId
    verificationKey <- lookupGetParam "key"
    if entrySubscriptionKey subscription == verificationKey && isJust (entrySubscriptionKey subscription)
        then do
            if entrySubscriptionVerified subscription
                then do
                    getSubscriptions $ entrySubscriptionEmail subscription
                else do
                    runDB $ update subscriptionId [EntrySubscriptionVerified =. True]
                    
                    setMessageI MsgSubscriptionVerified
                    getSubscriptions $ entrySubscriptionEmail subscription
        else do
            permissionDeniedI MsgPermissionDenied
            

postEditEntrySubscriptionR :: EntrySubscriptionId -> Handler Html
postEditEntrySubscriptionR subscriptionId = do
    ((result, _), _) <- runFormPost unsubscribeForm
    case result of
        FormSuccess verificationKey -> do
            subscription <- runDB $ get404 subscriptionId
            if entrySubscriptionKey subscription == verificationKey && isJust (entrySubscriptionKey subscription)
                then do
                    runDB $ delete subscriptionId
                    setMessageI MsgSubscriptionDeleted
                    getSubscriptions $ entrySubscriptionEmail subscription
                else do
                    permissionDeniedI MsgPermissionDenied
        
        FormFailure errors -> do
            setMessage [shamlet|
                $forall error <- errors
                    <p>#{error}
                |]
            redirectUltDest HomeR
        _ -> do
            setMessageI MsgFormMissing
            redirectUltDest HomeR


unsubscribeForm :: Form (Maybe Text)
unsubscribeForm = renderBootstrap3 BootstrapBasicForm $ aopt hiddenField keySettings Nothing
    where
        keySettings = FieldSettings {
            fsLabel = "",
            fsTooltip = Nothing,
            fsId = Nothing,
            fsName = Just "key",
            fsAttrs = [("autofocus", ""), ("placeholder", "Key"), ("class", "form-control")]
        }
