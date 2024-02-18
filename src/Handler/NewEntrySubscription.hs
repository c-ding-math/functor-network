{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.NewEntrySubscription where

import Import
import Handler.Parser(jqueryUiWidget)
import Yesod.Form.Bootstrap3
import Text.Shakespeare.Text 
import qualified Crypto.Nonce as Nonce
import System.IO.Unsafe (unsafePerformIO)

postNewEntrySubscriptionR ::EntryId -> Handler Html
postNewEntrySubscriptionR entryId= do

    entry <- runDB $ get404 entryId
    ((result, _), _) <- runFormPost $ subscribeEntryForm Nothing
    _<-case result of
        FormSuccess address -> do
            urlRenderParams <- getUrlRenderParams
            currentTime <- liftIO getCurrentTime
            verificationKey <- liftIO $ Nonce.nonce128urlT $ unsafePerformIO (Nonce.new)
            mCurrentUserId <- maybeAuthId
            mEmail <- runDB $ getBy $ UniqueEmail address 
            let  verified = case entityVal <$> mEmail of
                    Just email | isAdministrator mCurrentUserId email -> True
                    _ -> False
            
            eEntrySubscription <- runDB $ insertBy $ EntrySubscription{
                entrySubscriptionEmail = address,
                entrySubscriptionEntryId = entryId,
                entrySubscriptionKey = Just verificationKey,
                entrySubscriptionInserted = currentTime,
                entrySubscriptionVerified = verified
            }
            
            case eEntrySubscription of
                Left (Entity subscriptionId subscription) -> if entrySubscriptionVerified subscription
                    then do
                        setMessageI $ MsgAlreadySubscribed address
                    else do
                        
                        setMessageI $ MsgSubscriptionConfirmationSent address
                        
                        let url = case entrySubscriptionKey subscription of
                                Just key -> urlRenderParams (EditEntrySubscriptionR subscriptionId) [("key", key)]
                                Nothing -> urlRenderParams (EditEntrySubscriptionR subscriptionId) []
                            
                            emailSubject = "Subscription confirmation"
                            emailText = [stext|
Please confirm your subscription to the post "#{entryTitle entry}" with the link below.

#{url}

Thank you!

#{appName}
                            
                        |]
                            emailHtml = [shamlet|
<p>Please confirm your subscription to the post "#{entryTitle entry}" by clicking the link below.
<p>
    <a href=#{url}>Confirm subscription
<p>Thank you!
<p>#{appName}
                        |] 
                        sendSystemEmail address emailSubject emailText emailHtml
                Right subscriptionId -> if verified
                    then do  
                        setMessageI $ MsgEntrySubscriptionConfirmation address

                    else do
                        
                        setMessageI $ MsgSubscriptionConfirmationSent address 
                        
                        let url = urlRenderParams (EditEntrySubscriptionR subscriptionId) [("key", verificationKey)]
                            emailSubject = "Subscription confirmation"
                            emailText = [stext|
Please confirm your subscription to the post "#{entryTitle entry}" with the link below.

#{url}

Thank you!

#{appName}
                            
                        |]
                            emailHtml = [shamlet|
<p>Please confirm your subscription to the post "#{entryTitle entry}" by clicking the link below.
<p>
    <a href=#{url}>Confirm subscription
<p>Thank you!
<p>#{appName}
                        |] 
                        sendSystemEmail address emailSubject emailText emailHtml
            --redirect $ HomeR authorId
        FormFailure _ -> setMessageI MsgFormFailure
        _ -> setMessageI MsgFormMissing

    redirect $ UserEntryR (entryUserId entry) entryId

subscribeEntryForm ::Maybe Text -> Form Text 
subscribeEntryForm mEmail = renderBootstrap3 BootstrapBasicForm $ areq emailField (bfs MsgEmail) mEmail

subscribeToEntryWidget :: EntryId -> Widget
subscribeToEntryWidget entryId = do
    (subscribeWidget, subscribeEnctype) <- handlerToWidget $ do
        mCurrentUserId<-maybeAuthId
        mCurrentUserEmail <- runDB $ selectFirst [EmailUserId ==. mCurrentUserId, EmailVerified ==. True] [Desc EmailInserted]

        generateFormPost $ subscribeEntryForm $ (emailAddress . entityVal) <$> mCurrentUserEmail
    [whamlet|
        <a .subscribe href=#>_{MsgSubscribe}
        <form style="display:none;" #subscribe-form method=post action=@{NewEntrySubscriptionR entryId} enctype=#{subscribeEnctype}>
            <p>_{MsgSubscribeToEntry}
            ^{subscribeWidget}
    |]
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
    jqueryUiWidget