{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

data RoleType =  SuperAdministrator | Administrator 
    deriving (Show, Read, Eq)
derivePersistField "RoleType"

data SubscriptionType 
        = EntrySubscription --when a entry receive a comment
        | CommentSubscription --when a comment receive a reply
    deriving (Show, Read, Eq)
derivePersistField "SubscriptionType"

data EntryType =  Standard | Page | Comment
    deriving (Show, Read, Eq)
derivePersistField "EntryType"

data EntryStatus =  Publish | Draft 
    deriving (Show, Read, Eq)
derivePersistField "EntryStatus"

data Format = Format Text
    deriving (Show, Read, Eq, Generic)
instance FromJSON Format where
instance ToJSON Format where
derivePersistField "Format"

instance Ord RoleType where
    compare SuperAdministrator Administrator = GT
    compare Administrator SuperAdministrator = LT
    compare _ _ = EQ

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")
