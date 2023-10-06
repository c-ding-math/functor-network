{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.EditUserSubscription where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Handler.Subscriptions (getSubscriptions)

getEditUserSubscriptionR :: UserSubscriptionId -> Handler Html
getEditUserSubscriptionR subscriptionId = do
    subscription <- runDB $ get404 subscriptionId
    verificationKey <- lookupGetParam "key"
    if userSubscriptionKey subscription == verificationKey && isJust (userSubscriptionKey subscription)
        then do
            if userSubscriptionVerified subscription
                then do
                    getSubscriptions $ userSubscriptionEmail subscription
                else do
                    runDB $ update subscriptionId [UserSubscriptionVerified =. True]
                    
                    setMessageI MsgSubscriptionVerified
                    getSubscriptions $ userSubscriptionEmail subscription
        else do
            permissionDeniedI MsgPermissionDenied
            

postEditUserSubscriptionR :: UserSubscriptionId -> Handler Html
postEditUserSubscriptionR subscriptionId = do
    ((result, _), _) <- runFormPost unsubscribeForm
    case result of
        FormSuccess verificationKey -> do
            subscription <- runDB $ get404 subscriptionId
            if userSubscriptionKey subscription == verificationKey && isJust (userSubscriptionKey subscription)
                then do
                    runDB $ delete subscriptionId
                    setMessageI MsgSubscriptionDeleted
                    getSubscriptions $ userSubscriptionEmail subscription
                else do
                    permissionDeniedI MsgPermissionDenied
        
        FormFailure errors -> do
            setMessage [shamlet|
                $forall error <- errors
                    <p>#{error}
                |]
            redirectUltDest Home0R
        _ -> do
            setMessageI MsgFormMissing
            redirectUltDest Home0R

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
