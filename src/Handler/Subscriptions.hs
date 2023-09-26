{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Subscriptions where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getSubscriptionsR :: EmailId -> Handler Html
getSubscriptionsR emailId = do
    email <- runDB $ get404 emailId
    userId <- requireAuthId
    if emailUserId email == Just userId && emailVerified email
        then do
            getSubscriptions $ emailAddress email
        else do
            permissionDeniedI MsgPermissionDenied
    
getUserSubscriptionR :: UserSubscriptionId -> Handler Html
getUserSubscriptionR subscriptionId = do
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
            

postUserSubscriptionR :: UserSubscriptionId -> Handler Html
postUserSubscriptionR subscriptionId = do
    ((result, _), _) <- runFormPost userUnsubscribeForm
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


userUnsubscribeForm :: Form (Maybe Text)
userUnsubscribeForm = renderBootstrap3 BootstrapBasicForm $ aopt hiddenField keySettings Nothing
    where
        keySettings = FieldSettings {
            fsLabel = "",
            fsTooltip = Nothing,
            fsId = Nothing,
            fsName = Just "key",
            fsAttrs = [("autofocus", ""), ("placeholder", "Key"), ("class", "form-control")]
        }

getSubscriptions :: Text -> Handler Html
getSubscriptions address = do
    (userSubscriptionList,userList) <-runDB $ do
        userSubscriptionEntities <- selectList [UserSubscriptionEmail ==. address, UserSubscriptionVerified==.True] [Desc UserSubscriptionInserted]
        userEntities <- mapM (\x -> do
            let userId = userSubscriptionUserId $ entityVal x
            user <- get404 userId
            return $ Entity userId user
            ) userSubscriptionEntities
        return (userSubscriptionEntities,userEntities)
    (userUnsubscribeFormWidget, userUnsubscribeFormEnctype) <- generateFormPost userUnsubscribeForm
    defaultLayout $ do
        setTitleI MsgEmailSubscriptions
        [whamlet|
            <h1>_{MsgEmailSubscriptions}
            <label>_{MsgEmailAddress}: </label>
            <span>#{address}
            <!--<h3>_{MsgUsers}-->
            <p>_{MsgUserSubscriptionsDescription}
            $if null userSubscriptionList
                <p>_{MsgNoSubscription}
            $else
                <ul>
                    $forall (Entity subscriptionId subscription, Entity userId user)<- zip userSubscriptionList userList
                        <li>
                            <a href=@{HomeR userId}>#{userName user}
                            $maybe key <- userSubscriptionKey subscription
                                <ul .entry-menu.inline-menu>
                                    <li>
                                        <a .unsubscribe href=@{UserSubscriptionR subscriptionId} data-key=#{key}>_{MsgUnsubscribe}
                            $nothing
                                <ul .entry-menu.inline-menu>
                                    <li>
                                        <a .unsubscribe href=@{UserSubscriptionR subscriptionId} data-key="">_{MsgUnsubscribe}
                    <form .hidden #unsubscribe-form action="" method=post enctype=#{userUnsubscribeFormEnctype}>
                        ^{userUnsubscribeFormWidget}
                                    
        |]
        toWidget [julius|
            $(document).ready(function() {
                $(".unsubscribe").click(function(e) {
                    e.preventDefault();
                    var key = $(this).data("key");
                    var form = $("#unsubscribe-form");
                    form.attr("action", $(this).attr("href"));
                    form.find("input[name='key']").val(key);
                    form.submit();
                });
            });
        |]

