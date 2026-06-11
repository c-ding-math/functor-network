{-# LANGUAGE OverloadedStrings #-}
{-# Language QuasiQuotes #-}
{-# Language NoImplicitPrelude #-}

module Handler.Cache where

import Import
import Handler.Parse
import Yesod.Form.Bootstrap3
import Database.Persist.Sql (toSqlKey)

postIdForm :: Form (Maybe Int)
postIdForm = renderBootstrap3 BootstrapBasicForm $ aopt intField "Post ID" Nothing

getCacheR :: Handler Html
getCacheR = do
    (formWidget, enctype) <- generateFormPost postIdForm
    defaultLayout $ do
        setTitle "cache"
        [whamlet|
            <form method=post action=@{CacheR} enctype=#{enctype}>
                ^{formWidget}
                <button .btn .btn-primary type=submit>_{MsgSubmit}
        |]

postCacheR :: Handler Html
postCacheR = do
    ((result, _), _) <- runFormPost postIdForm
    case result of
        FormSuccess Nothing -> do
            entries <- runDB $ selectList [] []
            mapM_ (cacheEntry . entityKey) entries 
        FormSuccess (Just postId) -> do
            let entryId = toSqlKey $ fromIntegral postId
            cacheEntry entryId
        FormMissing -> do
            setMessageI MsgFormMissing
        FormFailure errors -> do
            setMessage [shamlet|
                $forall error <- errors
                    <p>#{error}
                |]
    redirect CacheR