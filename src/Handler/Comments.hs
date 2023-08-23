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
        commentEntryList'' <- mapM getCommentEntry comments
        let commentEntryList'=catMaybes commentEntryList''
            commentEntryList =[x | x<- commentEntryList', (entryStatus . entityVal . fst) x == Publish||isAdministrator mCurrentUserId ((entityVal . fst) x)] 
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
                        $forall  (Entity entryId entry, Entity commentId _) <- commentEntryList
                            <li :entryStatus entry == Draft:.draft>
                                <a href=@{EntryR piece entryId}#comment-#{toPathPiece commentId}>
                                    <h2>#{preEscapedToMarkup (scaleHeader 2 (entryOutputTitle entry))}
                                <div .tags>
                                    ^{tagsWidget (zip (entryInputTags entry) (entryOutputTags entry))}
        |]
        addStylesheet $ StaticR css_entry_list_css

getCommentEntry :: (BaseBackend backend ~ SqlBackend,PersistStoreRead backend, MonadHandler m) => Entity Entry -> ReaderT backend m (Maybe (Entity Entry, Entity Entry))
getCommentEntry commentEntity = do
    case (entryParentId  (entityVal commentEntity)) of
        Nothing -> return Nothing
        Just entryId -> do
            mEntry <- get $ entryId
            case mEntry of
                Nothing -> return Nothing
                Just entry -> return $ Just (Entity entryId entry, commentEntity)