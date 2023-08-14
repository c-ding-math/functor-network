{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Import
import Handler.Parser (tagsWidget)
import Parse.Parser (scaleHeader)

getHomeR :: Path -> Handler Html
getHomeR piece = do    
    mCurrentUserId<-maybeAuthId
    (recentEntryList, recentCommentEntryList)<-runDB $ do
        _<-get404 piece      
        entries<- selectList [EntryUserId==.piece,EntryType==.Standard] [Desc EntryInserted]
        let entryList =[x | x<-entries, entryStatus (entityVal x) == Publish||isAdministrator mCurrentUserId (entityVal x)]          
            
        comments <- selectList [EntryUserId==.piece,EntryType==.Comment] [Desc EntryInserted]
        commentEntryList'' <- mapM getCommentEntry comments
        let commentEntryList'=catMaybes commentEntryList''
            commentEntryList =[x | x<- commentEntryList', (entryStatus . entityVal . fst) x == Publish||isAdministrator mCurrentUserId ((entityVal . fst) x)]               
        return $ (take 5 entryList, take 5 commentEntryList)
    --search  entries comments tags
    defaultLayout $ do
        setTitleI MsgHome
        [whamlet|
        
        <div .search>
        <section .recent-entries>
            $if null recentEntryList
                <h2>_{MsgRecentEntries}
                    $if mCurrentUserId == Just piece
                        <a .navbar-right .btn.btn-default href=@{NewEntryR}>_{MsgNewPost} 
                <div> _{MsgNoPost} 
                    $if mCurrentUserId == Just piece                      
                        <a href=@{NewEntryR}>_{MsgFirstPost}
            $else
                <h2>_{MsgRecentEntries}
                    <a .navbar-right .btn.btn-default href=@{EntriesR piece}>_{MsgViewAll}
                    $if mCurrentUserId == Just piece
                        <span .navbar-right style="visibility: hidden;">|</span>
                        <a .navbar-right .btn.btn-default href=@{NewEntryR}>_{MsgNewPost}
                        
                    

                <div .entries>
                    <ul>
                        $forall Entity entryId entry<-recentEntryList
                            <li :entryStatus entry == Draft:.draft>
                                <a href=@{EntryR piece entryId}>
                                    <h3>#{preEscapedToMarkup (scaleHeader 3 (entryOutputTitle entry))}
                                <div .tags>
                                    ^{tagsWidget piece (zip (entryInputTags entry) (entryOutputTags entry))}

        <section .recent-comments>
            
                
            $if null recentCommentEntryList
                <h2>_{MsgRecentComments}
                <div> _{MsgNoComment}
            $else
                <h2>_{MsgRecentCommentsOn}
                    <a .navbar-right .btn.btn-default href=@{CommentsR piece}>_{MsgViewAll} 
                <div .entries>
                    <ul>
                        $forall  (Entity entryId entry, Entity commentId _) <- recentCommentEntryList
                            <li :entryStatus entry == Draft:.draft>
                                <a href=@{EntryR piece entryId}#comment-#{toPathPiece commentId}>
                                    <h3>#{preEscapedToMarkup (scaleHeader 3 (entryOutputTitle entry))}
                                <div .tags>
                                    ^{tagsWidget piece (zip (entryInputTags entry) (entryOutputTags entry))}
                                         
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
