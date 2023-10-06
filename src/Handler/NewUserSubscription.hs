{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.NewUserSubscription where

import Import
import Handler.Parser(jqueryUiWidget)
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
                userSubscriptionVerified = verified
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
                            
                            emailSubject = "Subscription confirmation"
                            emailText = [stext|
Please confirm your subscription to #{userName author} with the link below.

#{url}

Thank you!

#{appName}
                            
                        |]
                            emailHtml = [shamlet|
<p>Please confirm your subscription to #{userName author} by clicking the link below.
<p>
    <a href=#{url}>Confirm subscription
<p>Thank you!
<p>#{appName}
                        |] 
                        sendSystemEmail address emailSubject emailText emailHtml
                Right subscriptionId -> if verified
                    then do  
                        setMessageI $ MsgUserSubscriptionConfirmation address

                    else do
                        
                        setMessageI $ MsgSubscriptionConfirmationSent address
                        urlRenderParams <- getUrlRenderParams
                        
                        let url = urlRenderParams (EditUserSubscriptionR subscriptionId) [("key", verificationKey)]
                            emailSubject = "Subscription confirmation"
                            emailText = [stext|
Please confirm your subscription to #{userName author} with the link below.

#{url}

Thank you!

#{appName}
                            
                        |]
                            emailHtml = [shamlet|
<p>Please confirm your subscription to #{userName author} by clicking the link below.
<p>
    <a href=#{url}>Confirm subscription
<p>Thank you!
<p>#{appName}
                        |] 
                        sendSystemEmail address emailSubject emailText emailHtml
            --redirect $ HomeR authorId
        FormFailure _ -> setMessageI MsgFormFailure
        _ -> setMessageI MsgFormMissing
    redirect $ HomeR authorId


userSubscriptionForm ::Maybe Text -> Form Text 
userSubscriptionForm mEmail = renderBootstrap3 BootstrapBasicForm $ areq emailField (bfs MsgEmail) mEmail

subscribeToUserWidget :: UserId -> Widget
subscribeToUserWidget authorId = do
    (subscribeWidget, subscribeEnctype) <- handlerToWidget $ do
        mCurrentUserId<-maybeAuthId
        mCurrentUserEmail <- runDB $ selectFirst [EmailUserId ==. mCurrentUserId, EmailVerified ==. True] [Desc EmailInserted]

        generateFormPost $ userSubscriptionForm $ (emailAddress . entityVal) <$> mCurrentUserEmail
    [whamlet|
        <a .btn.btn-default .subscribe href=#>_{MsgSubscribe}
        <form style="display:none;" #subscribe-form method=post action=@{NewUserSubscriptionR authorId} enctype=#{subscribeEnctype}>
            <p>_{MsgSubscribeToUser}
            ^{subscribeWidget}
    |]
    jqueryUiWidget
    toWidget [julius|
$(document).ready(function(){
    $(".subscribe").click(function(e){
        e.preventDefault();
		var prompt = $("#subscribe-form");
		prompt.dialog({
			modal: true,
            title: "Subscribe",
			buttons: [
				{
					html: "Subscribe",
					class: "btn btn-primary",
					click: function () {
                        $(this).dialog("close");
                        $(this).submit();
                    },
				},
				{
					html: "Cancel",
					class:"btn btn-default",
					click: function () {
						$(this).dialog( "close" );
					},
				},
			]
		});        

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
    