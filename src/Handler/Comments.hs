{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Comments where

import Import
import Handler.Entries (entryListWidget)
import Handler.EditComment (getRootEntryId)

getCommentsR :: UserId -> Handler Html
getCommentsR piece = do
    mCurrentUserId<-maybeAuthId 
    entryList<-runDB $ do
        _<-get404 piece
        comments <- selectList [EntryUserId==.piece, EntryType==.Comment] [Desc EntryInserted]
        entryIds <- mapM getRootEntryId $ entityKey <$> comments
        entries <- selectList [EntryId <-. entryIds, EntryType==.UserPost] [Desc EntryInserted]
        return $ [x | x<-entries, entryStatus (entityVal x) == Publish||isAdministrator mCurrentUserId (entityVal x)]

    defaultLayout $ do
        setTitleI MsgComments
        [whamlet|        
            <h1>_{MsgComments}
                
            $if null entryList
                <div>_{MsgNoComment}
            $else
                <p>You have comments on the following posts:
                ^{entryListWidget "" entryList}
        |]
