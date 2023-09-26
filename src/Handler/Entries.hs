{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Entries where

import Import
import Parse.Parser (scaleHeader)
import Yesod.Form.Bootstrap3
import Text.Shakespeare.Text 
import qualified Crypto.Nonce as Nonce
import System.IO.Unsafe (unsafePerformIO)

getEntriesR :: UserId -> Handler Html
getEntriesR authorId = do
    mCurrentUserId<-maybeAuthId
    (entryList,author)<-runDB $ do
        author<-get404 authorId      
        entries<- selectList [EntryUserId==.Just authorId, EntryType==.Standard] [Desc EntryInserted]
        let entryList =[x | x<-entries, entryStatus (entityVal x) == Publish||isAdministrator mCurrentUserId (entityVal x)]          
        return $ (entryList,author)
    
    defaultLayout $ do
        setTitle $ toHtml $ userName author
        [whamlet|
            <div .page-header>       
                <h1>_{MsgPosts}
                <div .page-header-menu>
                    
                    ^{subscribeUserWidget authorId}
                    $if mCurrentUserId == Just authorId
                        <a .btn.btn-primary .new-entry href=@{NewEntryR}>_{MsgNewPost}
            $if null entryList
                <p>_{MsgNoPost} #
                    $if mCurrentUserId == Just authorId                      
                        <a href=@{NewEntryR}>_{MsgFirstPost}
            $else
                ^{entryListWidget entryList}
        |]
        toWidget [hamlet|
            <div style="display:none;"><a href=@{CommentsR authorId}>Comments</a></div>
        |]

entryListWidget :: [Entity Entry] -> Widget
entryListWidget entryList = do
    toWidget [hamlet|
    <div .entries>
        <ul>
            $forall Entity entryId entry<-entryList
                $maybe authorId <- entryUserId entry
                    <li :entryStatus entry == Draft:.draft>
                        <a href=@{EntryR authorId entryId}>
                            <h2 .entry-title>#{preEscapedToMarkup (scaleHeader 2 (entryOutputTitle entry))}
                        <div .tags>
                            <ul>
                                $forall (inputTag, outputTag) <- zip (entryInputTags entry) (entryOutputTags entry)
                                    <li>
                                        <a href=@{TagR inputTag}>#{preEscapedToMarkup outputTag}
    |]
    toWidget [lucius|

.entries>ul, .tags>ul {
    list-style:none;
    padding-left:0;
}

.entries>ul>li{
    border-top:1px solid #dce4ec;
    padding-bottom:1em;
}
.entries>ul>li>a>h2, .entries>ul>li>a>h3, .tags ul>li>a {
    color:black;
}

.tags ul>li{
    display:inline-block;
    padding:0.25em 0.5em;
    background-color:#eee;
    margin-bottom:1em;
}
li h1{
    color:black;
    font-size:2em;
}
    |]


postEntriesR :: UserId -> Handler Html --Email subscription
postEntriesR authorId = do
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
                                Just key -> urlRenderParams (UserSubscriptionR subscriptionId) [("key", key)]
                                Nothing -> urlRenderParams (UserSubscriptionR subscriptionId) []
                            
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
                        
                        let url = urlRenderParams (UserSubscriptionR subscriptionId) [("key", verificationKey)]
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

subscribeUserWidget :: UserId -> Widget
subscribeUserWidget authorId = do
    (subscribeWidget, subscribeEnctype) <- handlerToWidget $ do
        mCurrentUserId<-maybeAuthId
        mCurrentUserEmail <- runDB $ selectFirst [EmailUserId ==. mCurrentUserId, EmailVerified ==. True] [Desc EmailInserted]

        generateFormPost $ userSubscriptionForm $ (emailAddress . entityVal) <$> mCurrentUserEmail
    [whamlet|
        <a .btn.btn-default .subscribe href=#>_{MsgSubscribe}
        <form style="display:none;" #subscribe-form method=post action=@{HomeR authorId} enctype=#{subscribeEnctype}>
            <p>_{MsgSubscribeToUser}
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
    toWidget [lucius|
        .page-header{
            display:flex;
            justify-content:space-between;
            align-items:center;
        }
    |]
    addScript $ StaticR scripts_jquery_ui_jquery_ui_min_js
    addStylesheet $ StaticR scripts_jquery_ui_jquery_ui_min_css
    addStylesheet $ StaticR scripts_jquery_ui_jquery_ui_structure_min_css
    addStylesheet $ StaticR scripts_jquery_ui_jquery_ui_additional_css