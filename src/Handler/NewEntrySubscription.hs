{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.NewEntrySubscription where

import Import
import Yesod.Form.Bootstrap3
import Text.Shakespeare.Text 
import qualified Crypto.Nonce as Nonce
import System.IO.Unsafe (unsafePerformIO)
import Handler.Tree(getRootEntryId)

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
                        title <- runDB $ do
                                    mTree <- selectFirst [EntryTreeNode ==. entryId] []
                                    case mTree of
                                        Just (Entity _ tree) -> do
                                            parent <- get404 $ entryTreeParent tree
                                            case entryType parent of
                                                Category -> return $ entryTitle entry
                                                Comment -> do
                                                    parentAuthor <- get404 $ entryUserId parent
                                                    return $ "Reply to " <> userName parentAuthor
                                                _ -> return $ "Comment on " <> entryTitle parent
                                        Nothing -> return $ entryTitle entry
                                    
                        sendAppEmail address $ entrySubscriptionConfirmation url title
                Right subscriptionId -> if verified
                    then do  
                        setMessageI $ MsgEntrySubscriptionConfirmation address

                    else do
                        
                        setMessageI $ MsgSubscriptionConfirmationSent address 
                        
                        let url = urlRenderParams (EditEntrySubscriptionR subscriptionId) [("key", verificationKey)]
                        title <- runDB $ do
                                    mTree <- selectFirst [EntryTreeNode ==. entryId] []
                                    case mTree of
                                        Just (Entity _ tree) -> do
                                            parent <- get404 $ entryTreeParent tree
                                            case entryType parent of
                                                Comment -> do
                                                    parentAuthor <- get404 $ entryUserId parent
                                                    return $ "Reply to " <> userName parentAuthor
                                                _ -> return $ "Comment on " <> entryTitle parent
                                        Nothing -> return $ entryTitle entry
                        sendAppEmail address  $ entrySubscriptionConfirmation url title
            --redirect $ HomeR authorId
        FormFailure _ -> setMessageI MsgFormFailure
        _ -> setMessageI MsgFormMissing
    case entryType entry of
        Category -> redirect $ CategoriesR (entryUserId entry)-- :#: ("entry-" <> toPathPiece entryId)
        _ -> do 
            (rootEntryUserId, rootEntryId) <- runDB $ do 
                rootEntryId <- getRootEntryId entryId
                rootEntry <- get404 rootEntryId
                return (entryUserId rootEntry, rootEntryId)
            redirect $ UserEntryR rootEntryUserId rootEntryId

insertDefaultEntrySubscription :: EntryId -> ReaderT SqlBackend (HandlerFor App) ()
insertDefaultEntrySubscription entryId = do
    Entity _ user <- requireAuth

    case userEmail user of
        Just email -> do
            --urlRenderParams <- getUrlRenderParams
            currentTime <- liftIO getCurrentTime
            verificationKey <- liftIO $ Nonce.nonce128urlT $ unsafePerformIO (Nonce.new)
    
            insert_ $ EntrySubscription{
                entrySubscriptionEmail = email,
                entrySubscriptionEntryId = entryId,
                entrySubscriptionKey = Just verificationKey,
                entrySubscriptionInserted = currentTime,
                entrySubscriptionVerified = True
            }
            
        Nothing -> return ()


subscribeEntryForm ::Maybe Text -> Form Text 
subscribeEntryForm mEmail = renderBootstrap3 BootstrapBasicForm $ areq emailField (bfs MsgEmail) mEmail

subscribeToEntryWidget :: Widget
subscribeToEntryWidget = do
    messageRender <- getMessageRender
    (subscribeWidget, subscribeEnctype) <- handlerToWidget $ do
        mCurrentUser <- maybeAuth
        --mCurrentUserEmail <- runDB $ selectFirst [EmailUserId ==. mCurrentUserId, EmailVerified ==. True] [Desc EmailInserted]
        let mCurrentUserEmail = case mCurrentUser of
                Just (Entity _ user) -> userEmail user
                _ -> Nothing
        generateFormPost $ subscribeEntryForm $ mCurrentUserEmail
    --entry <- handlerToWidget $ runDB $ get404 entryId
    --let subscriptionFormId = "subscribe-form-" <> (toPathPiece entryId)
    [whamlet|
<div .modal.fade>
    <div .modal-dialog>
        <div .modal-content>
            <div .modal-header>
                <button type=button .close data-dismiss=modal>&times;
                <b .modal-title>_{MsgFollow}
            <div .modal-body>
                <form .subscription-form method=post enctype=#{subscribeEnctype}>
                    <p .subscription-form-text>
                    ^{subscribeWidget}
                    <div .text-right>
                        <button type=submit .btn.btn-primary>_{MsgFollow}
    |]
    toWidget [julius|
$(document).ready(function(){
    $(".subscribe a").click(function(e){
        e.preventDefault();
		var subscriptionForm = $(".subscription-form");
        subscriptionForm.attr("action", $(this).data("action"));
        if ($(this).closest(".category").length > 0){
            subscriptionForm.find(".subscription-form-text").text(#{messageRender MsgSubscribeToCategory});
        } else if ($(this).closest("#comments").length > 0){
            subscriptionForm.find(".subscription-form-text").text(#{messageRender MsgSubscribeToComment});
        } else {
            subscriptionForm.find(".subscription-form-text").text(#{messageRender MsgSubscribeToPost});
        }
        subscriptionForm.closest(".modal").modal("show");
    }); 
});
    |]

entrySubscriptionConfirmation :: Text -> Text -> AppEmail
entrySubscriptionConfirmation url title = AppEmail emailSubject emailText emailHtml
    where
        emailSubject = "Subscription confirmation"
        emailText = [stext|
Please confirm your subscription to "#{title}" with the link below.

#{url}

Thank you!

#{appName}
                            
        |]
        emailHtml = [shamlet|
<p>Please confirm your subscription to "#{title}" by clicking the link below.
<p>
    <a href=#{url}>Confirm subscription
<p>Thank you!
<p>#{appName}
        |]

entrySubscriptionNotification :: Text -> Text -> Entry -> AppEmail 
entrySubscriptionNotification unsubscribeUrl entryUrl entry = AppEmail emailSubject emailText emailHtml
    where
        entryBodyText = case entryBody entry of 
            Just (Textarea body) -> body
            _ -> "Coming soon..."

        emailSubject= entryTitle entry
        emailText = [stext|
New comment:

#{entryBodyText}

View it at #{entryUrl}.

Manage your subscriptions at #{unsubscribeUrl}.

#{appName}
            |]

        emailHtml = [shamlet|
<p>New comment:
<p>#{entryBodyText}
<p><a href=#{entryUrl}>View</a><span> | </span><a href=#{unsubscribeUrl}>Manage subscriptions</a>
<p>#{appName}
            |]



