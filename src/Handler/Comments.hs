{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Comments where

import Import
import Handler.Parser (tagsWidget)
import Parse.Parser (scaleHeader)

getCommentsR :: UserId -> Handler Html
getCommentsR piece = do
    mCurrentUserId<-maybeAuthId
    commentEntryList<-runDB $ do
        _<-get404 piece      
        comments <- selectList [EntryUserId==.piece,EntryType==.Comment] [Desc EntryInserted]
        commentEntryList' <- mapM getEntryCommentList comments
        let commentEntryList =[x | x<- commentEntryList', (entryStatus . entityVal . fst) x == Publish||isAdministrator mCurrentUserId ((entityVal . fst) x)]               
        return $ commentEntryList
    defaultLayout $ do
        setTitleI MsgCommentsOn
        [whamlet|        
            <h1>_{MsgCommentsOn}
                
            $if null commentEntryList
                <div> _{MsgNoComment}
            $else
                <div .entries>
                    <ul>
                        $forall  (Entity entryId entry, Entity commentId comment) <- commentEntryList
                            <li :entryStatus entry == Draft:.draft>
                                <a href=@{EntryR piece entryId}#comment-{toPathPiece commentId}>
                                    <h2>#{preEscapedToMarkup (scaleHeader 2 (entryOutputTitle entry))}
                                <div .tags>
                                    ^{tagsWidget piece (zip (entryInputTags entry) (entryOutputTags entry))}
        |]
        $(widgetFile "entry-list")

getEntryCommentList :: (BaseBackend backend ~ SqlBackend,PersistStoreRead backend, MonadHandler m) => Entity Entry -> ReaderT backend m (Entity Entry, Entity Entry)
getEntryCommentList commentEntity = do
    let mEntryId = entryParentId $ entityVal commentEntity
    case mEntryId of
        Just entryId -> do
            entry <- get404 $ entryId
            return (Entity entryId entry, commentEntity)
        Nothing -> notFound
