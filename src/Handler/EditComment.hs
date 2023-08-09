module Handler.EditComment where

import Import

postEditCommentR :: EntryId -> Handler ()
postEditCommentR commentId = runDB $ do
    maybeUserId<-maybeAuthId
    comment <- get404 commentId
    mEntry<-mapM get404 $ entryParentId comment

    case (isAdministrator maybeUserId comment, mEntry) of
        (True, _) -> delete commentId
        (_, Just entry) | isAdministrator maybeUserId entry -> delete commentId
        _ -> permissionDeniedI MsgPermissionDenied
        
    
            --setMessageI MsgCommentDeleted
            {-case entrySiteId entry of
                Just sid-> do
                    site<-get404 sid
                    redirect $ EntryR (sitePath site) (commentEntryId comment)
                    --redirect $ EditCommentR commentId
                _ ->  notFound-}


    