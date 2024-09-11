{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Subscriptions where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Handler.EditComment(commentLink)

getSubscriptionsR :: EmailId -> Handler Html
getSubscriptionsR emailId = do
    email <- runDB $ get404 emailId
    userId <- requireAuthId
    if emailUserId email == Just userId && emailVerified email
        then do
            getSubscriptions $ emailAddress email
        else do
            permissionDeniedI MsgPermissionDenied
    
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

getSubscriptions :: Text -> Handler Html
getSubscriptions address = do
    (userSubscriptionList,userList,entrySubscriptionList,entryList) <-runDB $ do
        userSubscriptionEntities <- selectList [UserSubscriptionEmail ==. address, UserSubscriptionVerified==.True] [Desc UserSubscriptionInserted]
        entrySubscriptionEntities <- selectList [EntrySubscriptionEmail ==. address, EntrySubscriptionVerified==.True] [Desc EntrySubscriptionInserted]
        userEntities <- mapM (\x -> do
            let userId = userSubscriptionUserId $ entityVal x
            user <- get404 userId
            return $ Entity userId user
            ) userSubscriptionEntities
        entryEntities <- mapM (\x -> do
            let entryId = entrySubscriptionEntryId $ entityVal x
            entry <- get404 entryId
            return $ Entity entryId entry
            ) entrySubscriptionEntities
        return (userSubscriptionEntities,userEntities,entrySubscriptionEntities,entryEntities)
    let postList = [ x | x <- entryList, entryType (entityVal x) == UserPost]
    let commentList = [ x | x <- entryList, entryType (entityVal x) == Comment]
    let feedbackList = [ x | x <- entryList, entryType (entityVal x) == Feedback]
    entryLinkList <- runDB $ do
        mapM (\(Entity entryId entry) -> do
            case entryType entry of
                Comment -> do
                    theLink <- commentLink entryId
                    return theLink
                _ -> return ("","")
            ) entryList
    (unsubscribeFormWidget, unsubscribeFormEnctype) <- generateFormPost unsubscribeForm
    defaultLayout $ do
        setTitleI MsgEmailSubscriptions
        [whamlet|
            <h1>_{MsgEmailSubscriptions}
            <label>_{MsgEmailAddress}: </label>
            <span>#{address}
            <h3>_{MsgUsers}
            <p>_{MsgUserSubscriptionsDescription}
            $if null userSubscriptionList
                <p>_{MsgNoSubscription}
            $else
                <ul>
                    $forall (Entity subscriptionId subscription, Entity userId user)<- zip userSubscriptionList userList
                        <li>
                            <a href=@{UserHomeR userId}>#{userName user}
                            $maybe key <- userSubscriptionKey subscription
                              <span.menu>
                                <ul.list-inline.text-lowercase>
                                    <li>
                                        <a.text-muted .unsubscribe href=@{EditUserSubscriptionR subscriptionId} data-key=#{key}>_{MsgUnsubscribe}
                            $nothing
                              <span.menu>
                                <ul.list-inline.text-lowercase>
                                    <li>
                                        <a.text-muted .unsubscribe href=@{EditUserSubscriptionR subscriptionId} data-key="">_{MsgUnsubscribe}

            <h3>_{MsgPosts}
            <p>_{MsgPostSubscriptionsDescription}
            $if null postList
                <p>_{MsgNoSubscription}
            $else
                <ul>
                    $forall (Entity subscriptionId subscription, Entity entryId entry)<- zip entrySubscriptionList entryList
                      $if entryType entry == UserPost
                        <li>
                          
                            <a href=@{UserEntryR (entryUserId entry) entryId}>#{preEscapedToMarkup $ entryTitleHtml entry}
                            $maybe key <- entrySubscriptionKey subscription
                              <span.menu>
                                <ul.list-inline.text-lowercase>
                                    <li>
                                        <a.text-muted .unsubscribe href=@{EditEntrySubscriptionR subscriptionId} data-key=#{key}>_{MsgUnsubscribe}
                            $nothing
                              <span.menu>
                                <ul.list-inline.text-lowercase>
                                    <li>
                                        <a.text-muted .unsubscribe href=@{EditEntrySubscriptionR subscriptionId} data-key="">_{MsgUnsubscribe}

            <h3>_{MsgComments}
            <p>_{MsgCommentSubscriptionsDescription}
            $if null commentList
                <p>_{MsgNoSubscription}
            $else
                <ul>
                    $forall (Entity subscriptionId subscription, Entity _ entry, (url, text))<- zip3 entrySubscriptionList entryList entryLinkList 
                      $if entryType entry == Comment
                        <li>
                          
                            <a href=#{url}>#{text}
                            $maybe key <- entrySubscriptionKey subscription
                              <span.menu>
                                <ul.list-inline.text-lowercase>
                                    <li>
                                        <a.text-muted .unsubscribe href=@{EditEntrySubscriptionR subscriptionId} data-key=#{key}>_{MsgUnsubscribe}
                            $nothing
                              <span.menu>
                                <ul.list-inline.text-lowercase>
                                    <li>
                                        <a.text-muted .unsubscribe href=@{EditEntrySubscriptionR subscriptionId} data-key="">_{MsgUnsubscribe}
            <h3>_{MsgOtherSubscriptions}
            
            $if null feedbackList
                <p>_{MsgNoSubscription}
            $else
                <ul>
                    $forall (Entity subscriptionId subscription, Entity entryId entry)<- zip entrySubscriptionList entryList
                      $if entryType entry == Feedback
                        <li>
                          
                            <a href=@{FeedbackR}#entry-#{toPathPiece entryId}>#{preEscapedToMarkup $ entryTitleHtml entry}
                            $maybe key <- entrySubscriptionKey subscription
                              <span.menu>
                                <ul.list-inline.text-lowercase>
                                    <li>
                                        <a.text-muted .unsubscribe href=@{EditEntrySubscriptionR subscriptionId} data-key=#{key}>_{MsgUnsubscribe}
                            $nothing
                              <span.menu>
                                <ul.list-inline.text-lowercase>
                                    <li>
   
            <form .hidden #unsubscribe-form action="" method=post enctype=#{unsubscribeFormEnctype}>
                ^{unsubscribeFormWidget}
                                    
        |]
        toWidget [lucius|
            .menu ul {
                display: inline-block;
                margin-left:2em;
            }
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

