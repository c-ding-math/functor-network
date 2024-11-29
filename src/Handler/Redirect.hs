{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Redirect where

import Import

getRedirectR :: Handler Html
getRedirectR = do
    userId <- requireAuthId
    redirect $ UserEntriesR userId
