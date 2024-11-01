{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.NewUserSubscription where

import Import
import Yesod.Form.Bootstrap3
import Text.Shakespeare.Text 
import qualified Crypto.Nonce as Nonce
import System.IO.Unsafe (unsafePerformIO)

postNewUserSubscriptionR :: UserId -> Handler Html
postNewUserSubscriptionR authorId = do
    author <- runDB $ get404 authorId
    ((result, _), _) <- runFormPost $ userSubscriptionForm Nothing
    _<-case result of
        FormSuccess address -> do
            forwardedFor <- lookupHeader "X-Forwarded-For"
            currentTime <- liftIO getCurrentTime
            verificationKey <- liftIO $ Nonce.nonce128urlT $ unsafePerformIO (Nonce.new)
            mCurrentUserId <- maybeAuthId
            mEmail <- runDB $ getBy $ UniqueEmail address 
            let  verified = case entityVal <$> mEmail of
                    Just email | isAdministrator mCurrentUserId email -> True
                    _ -> False
            
            eUserSubscription <- runDB $ insertBy $ UserSubscription{
                userSubscriptionEmail = address,
                userSubscriptionUserId = authorId,
                userSubscriptionKey = Just verificationKey,
                userSubscriptionInserted = currentTime,
                userSubscriptionVerified = verified,
                userSubscriptionClient = show <$> forwardedFor
            }
            case eUserSubscription of
                Left (Entity subscriptionId subscription) -> if userSubscriptionVerified subscription
                    then do
                        setMessageI $ MsgAlreadySubscribed address
                    else do
                        
                        setMessageI $ MsgSubscriptionConfirmationSent address
                        urlRenderParams <- getUrlRenderParams
                        
                        let url = case userSubscriptionKey subscription of
                                Just key -> urlRenderParams (EditUserSubscriptionR subscriptionId) [("key", key)]
                                Nothing -> urlRenderParams (EditUserSubscriptionR subscriptionId) []
                            
                        sendAppEmail address $ userSubscriptionConfirmation url author
                Right subscriptionId -> if verified
                    then do  
                        setMessageI $ MsgUserSubscriptionConfirmation address

                    else do
                        
                        setMessageI $ MsgSubscriptionConfirmationSent address
                        urlRenderParams <- getUrlRenderParams
                        
                        let url = urlRenderParams (EditUserSubscriptionR subscriptionId) [("key", verificationKey)]
                        sendAppEmail address $ userSubscriptionConfirmation url author
        FormFailure _ -> setMessageI MsgFormFailure
        _ -> setMessageI MsgFormMissing
    redirect $ UserEntriesR authorId


userSubscriptionForm ::Maybe Text -> Form Text 
userSubscriptionForm mEmail = renderBootstrap3 BootstrapBasicForm $ areq emailField (bfs MsgEmail) mEmail

subscribeToUserWidget :: UserId -> Widget
subscribeToUserWidget authorId = do
    mCurrentUserId<-maybeAuthId
    (subscribeWidget, subscribeEnctype) <- handlerToWidget $ do
        --mCurrentUserId<-maybeAuthId
        mCurrentUserEmail <- runDB $ selectFirst [EmailUserId ==. mCurrentUserId, EmailVerified ==. True] [Desc EmailInserted]

        generateFormPost $ userSubscriptionForm $ (emailAddress . entityVal) <$> mCurrentUserEmail
    [whamlet|
        <a .btn.btn-default :mCurrentUserId /= Just authorId:.btn-primary .subscribe href=#>_{MsgSubscribe}
        <div .modal.fade>
            <div .modal-dialog>
                <div .modal-content>
                    <div .modal-header>
                        <button type=button .close data-dismiss=modal>&times;
                        <b .modal-title>_{MsgSubscribe}
                    <div .modal-body>
                        <form method=post action=@{NewUserSubscriptionR authorId} enctype=#{subscribeEnctype}>
                            <p>_{MsgSubscribeToUser}
                            ^{subscribeWidget}
                            <.text-right>
                                <button type=submit .btn.btn-primary>_{MsgSubscribe}
    |]
    toWidget [julius|
$(document).ready(function(){
    $(".subscribe").click(function(e){
        e.preventDefault();
        var modal = $(this).next();
        modal.modal("show");
    }); 
});
    |]
    toWidget [lucius|
        .page-header{
            display:flex;
            justify-content:space-between;
            align-items:center;
        }
    |]
    
userSubscriptionConfirmation :: Text -> User -> AppEmail
userSubscriptionConfirmation url author = AppEmail emailSubject emailText emailHtml
    where
        emailSubject = "Subscription confirmation"
        emailText = [stext|
Please confirm your subscription to the author #{userName author} with the link below.

#{url}

Thank you!

#{appName}                   
        |]
        emailHtml= [shamlet|
<p>Please confirm your subscription to the author #{userName author} by clicking the link below.
<p>
    <a href=#{url}>Confirm subscription
<p>Thank you!
<p>#{appName}
        |]

userSubscriptionNotification :: Text -> Text -> Text -> User -> AppEmail
userSubscriptionNotification unsubscribeUrl entryUrl title user = AppEmail emailSubject emailText emailHtml
    where
        emailSubject = "New post by " ++ (userName user)
        emailText= [stext|
The author #{userName user} you subscribed to has published a new post:

#{title}.

You can view the post at #{entryUrl}.
To unsubscribe, please visit #{unsubscribeUrl}.
#{appName}
                    |]
        emailHtml= [shamlet|
<p>The author #{userName user} you subscribed to has published a new post:
<p>#{title}
<p><a href=#{entryUrl}>Go to view</a><span> | </span><a href=#{unsubscribeUrl}>Manage subscriptions</a>
<p>#{appName}
                    |]