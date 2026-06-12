{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.CacheEntrySpec where

import TestImport
import Handler.Parse
import qualified Data.Text.IO as TIO
import System.Directory

spec :: Spec
spec = withApp $ do
    describe "cache entry" $ do
        it "test" $ do
          users <- runDB $ selectList ([] :: [Filter User]) []
          assertEq "user table empty" 0 $ length users