{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Following where

import Import
import Yesod.Form.Bootstrap3 

postFollowingR :: UserId -> EntryId -> Handler Html
postFollowingR _ entryId = runDB $  do
    userId <- requireAuthId
    currentTime <- liftIO getCurrentTime
    mFollowing <- getBy $ UniqueFollowing userId entryId 
    case mFollowing of
        Just (Entity followingId _) -> do
            deleteCascade followingId
            return [shamlet|follow|]
        Nothing -> do
            insert_ $ Following userId entryId currentTime
            return [shamlet|following|]

    {-((result, _), _)<-runFormPost $ followForm entryId
    case result of
        FormSuccess follow -> 
            runDB $ do 
                eFollowing <- insertBy $ follow
                case eFollowing of
                    Left (Entity followingId _) -> do
                        deleteCascade followingId
                        return [shamlet|follow|]
                    Right _ -> return [shamlet|following|]
        _ -> return [shamlet|error|]
        -}

followForm :: EntryId -> Form Following
followForm entryId = renderBootstrap3 BootstrapBasicForm $ Following
    <$> lift requireAuthId
    <*> pure entryId
    <*> lift (liftIO getCurrentTime)

followWidget :: Maybe UserId -> EntryId -> Widget
followWidget maybeUserId entryId = do
    --maybeUserId <- handlerToWidget maybeAuthId
    case maybeUserId of 
        Nothing -> [whamlet|
            <a href=@{AuthR LoginR}>
                _{MsgFollow}
            |]
        Just userId -> do
            mFollowing <- handlerToWidget $ runDB $ getBy $ UniqueFollowing userId entryId
            [whamlet|
                <a .follow href=@{FollowingR userId entryId}>
                    $maybe _ <- mFollowing
                        _{MsgFollowing}
                    $nothing
                        _{MsgFollow}

            |]
