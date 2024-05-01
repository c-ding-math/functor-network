{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Notifications where

import Import
--import Yesod.Form.Bootstrap3

data NotificationSetting = NotificationSetting
    { _filter :: [NotificationType]
    }

notificationFilterForm :: Maybe NotificationSetting -> Form NotificationSetting
notificationFilterForm mFilter = renderDivs $ NotificationSetting
    <$> areq (checkboxesFieldList notificationTypeList) "Notification filter" (_filter <$> mFilter) where
        notificationTypeList = [(MsgNotifyMeWhenCommented, Commented), (MsgNotifyMeWhenFollowed, Followed), (MsgNotifyMeWhenSubscribed, Subscribed), (MsgNotifyMeWhenSystemChanged, SystemChanged)]

    
getNotificationsR :: UserId -> Handler Html
getNotificationsR _ = do 
    (Entity _ user) <- requireAuth
    let notificationFilter= userNotification user
    notifications <- runDB $ selectList [NotificationType <-. notificationFilter] [Desc NotificationInserted]
    (notificationFilterWidget, notificationFilterEnctype) <- generateFormPost $ notificationFilterForm $ Just $ NotificationSetting notificationFilter
    defaultLayout $ do
        --setTitleI MsgNotifications
        [whamlet|
            <div .page-header>       
                <h1>_{MsgNotifications}
                <div .page-header-menu>

            <form .notification-filter-form method=post enctype=#{notificationFilterEnctype}>
                ^{notificationFilterWidget}
                
                <button type="submit" .btn.btn-primary>_{MsgFilter}
            
                    
            $if null notifications
                <p>_{MsgNoNotification} #
            $else
                <div .notifications>
                    <ul>
                        $forall (Entity notificationId notification) <- notifications
                            <li>
                                <div .notification-meta>
                                    <span .at>#{utcToString $ notificationInserted notification}
                                
                                    <h3 .notification-title>#{notificationTitleHtml notification}
                                    <div .notification-content>#{notificationBodyHtml notification}
        |]



postNotificationsR :: UserId -> Handler Html
postNotificationsR userId = do
    ((result, _), _) <- runFormPost $ notificationFilterForm Nothing
    case result of
        FormSuccess notificationSetting -> do
            runDB $ update userId [UserNotification =. _filter notificationSetting]   
        FormFailure _ -> setMessageI MsgFormFailure
        FormMissing -> setMessageI MsgFormMissing
    redirect $ NotificationsR userId