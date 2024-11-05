{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.LoginSetting where

import Import

getLoginSettingR :: LoginId -> Handler Html
getLoginSettingR _ = do
    theLast<-theLastLogin
    defaultLayout $ if theLast
        then [whamlet|
            <form method=post>
                <p>_{MsgTheLastLoginMethod}
                <button  .btn .btn-primary type=submit name=action value=cancel>_{MsgOkay}
        |]
        else[whamlet|
            <form method=post>
                <p>_{MsgLoginDeletionConfirmation}
                <button  .btn .btn-primary type=submit name=action value=cancel>_{MsgCancel}
                <button  .btn .btn-default type=submit name=action value=delete>_{MsgSure}   
        |]
    
postLoginSettingR :: LoginId -> Handler Html
postLoginSettingR loginId = do
    action<-lookupPostParam "action"
    case action of 
        Just "delete"->do 
            runDB $ delete loginId 
            setMessageI MsgLoginDeleted
            redirect SettingsR
        _->redirect SettingsR

getEmailSettingR :: EmailId -> Handler Html
getEmailSettingR _ = do
    theLast<-theLastLogin
    defaultLayout $ if theLast
        then [whamlet|
            <form method=post>
                <p>_{MsgTheLastLoginMethod}
                <button  .btn .btn-default type=submit name=action value=cancel>_{MsgOkay}
        |]
        else[whamlet|
            <form method=post>
                <p>_{MsgEmailDeletionConfirmation}
                <button  .btn .btn-primary type=submit name=action value=cancel>_{MsgCancel}
                <button  .btn .btn-default type=submit name=action value=delete>_{MsgSure}   
        |]
    
postEmailSettingR :: EmailId -> Handler Html
postEmailSettingR emailId = do
    theLast<-theLastLogin
    if theLast
        then do 
            setMessageI MsgTheLastLoginMethod
            redirect SettingsR
        else do
            Entity userId user<-requireAuth
            action<-lookupPostParam "action"
            case action of 
                Just "delete"->do 
                    runDB $ do 
                        email<-get404 emailId
                        if userEmail user == (Just (emailAddress email))
                            then do 
                                update userId [UserEmail=.Nothing]
                                delete emailId 
                            else delete emailId
                    setMessageI MsgEmailDeleted
                    redirect SettingsR
                _->redirect SettingsR

theLastLogin::Handler Bool
theLastLogin=runDB $ do
    userId<-requireAuthId
    emails<-selectList[EmailUserId==.Just userId, EmailVerified==.True][]
    logins<-selectList[LoginUserId==.Just userId, LoginVerified==.True][]
    return $ if length emails + length logins==1
        then True
        else False