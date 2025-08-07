{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Data where

import Import
import Data.Aeson
import Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as V


data UserData = UserData
    { version :: String
    , user :: Entity User
    , emails :: [Entity Email]
    , logins :: [Entity Login]
    , entries :: [Entity Entry]
    , files :: [Entity File]
    , votes :: [Entity Vote]
    , entryTrees :: [Entity EntryTree]
    }
    
instance ToJSON UserData where
    toJSON (UserData _version _user _emails _logins _entries _files _votes _trees) =
        object
            [ "version" .= _version
            , "user" .= _user
            , "emails" .= _emails
            , "logins" .= _logins
            , "entries" .= _entries
            , "files" .= _files
            , "votes" .= _votes
            , "entryTrees" .= _trees
            ]

getDataR :: Handler Value
getDataR = do
    Entity userId u <- requireAuth
    (entryList, fileList, emailList, loginList, voteList, treeList) <- runDB $ do
        fileList <- selectList [FileUserId ==. userId] []
        entryList <- selectList [EntryUserId ==. userId] []
        emailList <- selectList [EmailUserId ==. Just userId] []
        loginList <- selectList [LoginUserId ==. Just userId] []
        voteList <- selectList [VoteUserId ==. userId] []
        let categoryList = Import.filter (\(Entity _ e) -> entryType e == Category) entryList
            commentList = Import.filter (\(Entity _ e) -> entryType e == Comment) entryList
            feedbackList = Import.filter (\(Entity _ e) -> entryType e == Feedback) entryList
        categoryTreeList <- selectList [EntryTreeParent <-. Import.map entityKey categoryList] []
        otherTreeList <- selectList [EntryTreeNode <-. Import.map entityKey (commentList ++ feedbackList)] []

        return (entryList, fileList, emailList, loginList, voteList, categoryTreeList ++ otherTreeList)

    let dataVersion = "3.4.0" 
    let userData = UserData
            { version = dataVersion
            , user = Entity userId u
            , emails = emailList
            , logins = loginList
            , entries = entryList
            , files = fileList
            , votes = voteList
            , entryTrees = treeList
            }
    let res = removeKeyRecursive "titleHtml" $ removeKeyRecursive "bodyHtml" $ removeKeyRecursive "password" $ removeKeyRecursive "userId" $ toJSON userData 
    return res

-- Function to recursively remove a key from a Value
removeKeyRecursive :: Data.Aeson.Key -> Value -> Value
removeKeyRecursive key (Object obj) =
  -- Remove the key from the current object and recursively apply the function to its values
  Object (KeyMap.map (removeKeyRecursive key) (KeyMap.delete key obj))
removeKeyRecursive key (Array arr) =
  -- Recursively apply the function to all elements in the array
  Array (V.map (removeKeyRecursive key) arr)
removeKeyRecursive _ val = val  -- Base case: if it's not an object or array, return the value as is
