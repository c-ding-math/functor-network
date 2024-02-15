{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Handler.Account where

import Import
import Yesod.Form.Bootstrap3
import Handler.EditComment (deleteEntryRecursive)

confirmationForm :: Form Text
confirmationForm = renderBootstrap3 BootstrapBasicForm $ areq textField (bfs ("Confirmation"::Text)) Nothing

getAccountR :: Handler Html
getAccountR = do
    
    (widget, enctype) <- generateFormPost confirmationForm
    defaultLayout $ do
        setTitleI MsgAccount
        [whamlet|
            <h1>_{MsgAccount}
            <p>Not satisfied with our platform? A better way than just leaving is to <a href=@{FeedbackR}>give us feedback</a>. We are always looking for ways to improve.
            <p>_{MsgUnregistrationWarning}
            <blockquote>_{MsgUserUnregistrationConfirmation}
            <form method=post enctype=#{enctype}>
                ^{widget}
                <button .btn .btn-danger type=submit>_{MsgUnregister}
                <a .btn .btn-primary href=@{SettingsR}>_{MsgCancel}    
        |]

postAccountR :: Handler Html
postAccountR = do
    userId <- requireAuthId
    ((result, _), _) <- runFormPost confirmationForm
    case result of
        FormSuccess confirmation -> do
            msgRender <- getMessageRender
            if confirmation == msgRender MsgUserUnregistrationConfirmation
                then do
                    runDB $ deleteUserRecursive userId
                    setMessageI MsgUnregistrationSuccess
                    redirect FeedbackR
                    {-defaultLayout $ do
                        setTitleI MsgAccount
                        [whamlet|
                            <h1>_{MsgAccount}
                            <p>_{MsgUnregistrationSuccess}
                        |]-}
                else do
                    setMessageI MsgPleaseConfirmUnregistration
                    redirect $ AccountR
        _ -> do
            setMessageI MsgUnregistrationFailure
            redirect $ AccountR

deleteUserRecursive::UserId->ReaderT SqlBackend (HandlerFor App) ()
deleteUserRecursive userId =do
        entries <- selectList [EntryUserId ==. userId] []
        forM_ entries $ \entry -> do
            deleteEntryRecursive $ entityKey entry
        deleteWhere [UserSubscriptionUserId ==. userId]
        deleteWhere [FileUserId ==. userId]
        deleteWhere [LoginUserId ==.Just userId]
        deleteWhere [EmailUserId ==. Just userId]
        delete userId
