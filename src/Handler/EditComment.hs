{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.EditComment where

import Import
import Yesod.Form.Bootstrap3
import Handler.Parser(parse,userTemporaryDirectory)
import Handler.NewEntrySubscription(entrySubscriptionNotification,insertDefaultEntrySubscription)
import Parse.Parser(mdToHtml,texToHtml,EditorData(..))
import Handler.Tree(getRootEntryId)
--import Text.Shakespeare.Text

deleteEditCommentR :: EntryId -> Handler ()
deleteEditCommentR commentId = runDB $ do
    maybeUserId<-maybeAuthId
    comment <- get404 commentId
    rootEntryId <- getRootEntryId commentId
    mRootEntry <- get rootEntryId

    case (isAdministrator maybeUserId comment, mRootEntry) of
        (_, Just entry) | isAdministrator maybeUserId entry -> deleteEntryRecursive commentId
        (True, _) -> deleteEntryRecursive commentId
        _ -> permissionDeniedI MsgPermissionDenied

data CommentInput=CommentInput
    {preamble::Maybe Textarea
    ,inputFormat::Format
    ,body::Textarea
    ,citation::Maybe Textarea
    }

newCommentForm :: Maybe CommentInput -> Form CommentInput
newCommentForm mCommentData =  renderBootstrap3 BootstrapBasicForm $ CommentInput
    <$> aopt textareaField preambleSettings (preamble <$> mCommentData)
    <*> areq (selectFieldList inputFormats) formatSettings (inputFormat <$> mCommentData)
    <*> areq textareaField editorSettings (body <$> mCommentData)
    <*> aopt textareaField citationSettings (citation <$> mCommentData)
    where   
            inputFormats = [(MsgMarkdownWithLaTeX, Format "md"), (MsgPureLaTeX, Format "tex")]
            formatSettings =  FieldSettings
                { fsLabel = SomeMessage MsgComment
                , fsTooltip = Nothing
                , fsId = Nothing
                , fsName = Just "format"
                , fsAttrs =[("class", "input-sm form-control format-selector")]
                }
            editorSettings = FieldSettings
                { fsLabel = ""
                , fsTooltip = Nothing
                , fsId = Nothing
                , fsName = Just "content"
                , fsAttrs =
                    [ ("class", "editor form-control")
                    , ("placeholder", "")
                    ]
                }
            preambleSettings = FieldSettings
                { fsLabel = ""
                , fsTooltip = Nothing
                , fsId = Nothing
                , fsName = Just "preamble"
                , fsAttrs =[("class", "hidden")]
                }
            citationSettings = FieldSettings
                { fsLabel = ""
                , fsTooltip = Nothing
                , fsId = Nothing
                , fsName = Just "citation"
                , fsAttrs =[("class", "hidden")]             
                }

postEditCommentR :: EntryId -> Handler ()
postEditCommentR entryId = do
    Entity userId _ <- requireAuth
    (rootEntryId, rootEntryAuthorId, entry, entryAuthorName) <- runDB $ do
        rootEntryId <- getRootEntryId entryId
        entry <- get404 entryId
        entryAuthor <- get404 $ entryUserId entry
        rootEntryAuthorId <- entryUserId <$> get404 rootEntryId  
   
        return (rootEntryId, rootEntryAuthorId, entry, userName entryAuthor)  

    urlRenderParams <- getUrlRenderParams
    
    ((res, _), _) <- runFormPost $ newCommentForm $ Nothing
    case res of
        FormSuccess newCommentFormData -> do
            let editorData=EditorData{
                    editorPreamble=preamble newCommentFormData
                    ,editorContent=Just $ body newCommentFormData
                    ,editorCitation=citation newCommentFormData
                }
            currentTime <- liftIO getCurrentTime
            let parser=  case inputFormat newCommentFormData of
                        Format "tex" -> texToHtml
                        _ -> mdToHtml
            userDir<-userTemporaryDirectory
            let title = "Comment"
                
            bodyHtml <- liftIO $ parse userDir parser editorData
            let commentData=Entry 
                        {entryUserId=userId
                        ,entryType=Comment
                        ,entryFormat=inputFormat newCommentFormData
                        ,entryTitle=title
                        ,entryTitleHtml=title
                        ,entryPreamble=(preamble newCommentFormData)
                        ,entryBody=Just $ body newCommentFormData
                        ,entryBodyHtml=bodyHtml
                        ,entryCitation=citation newCommentFormData
                        ,entryInserted=currentTime
                        ,entryUpdated=currentTime
                        ,entryStatus=Publish
                        ,entryFeatured=False
                        }
                
            commentId <- runDB $ do
                commentId<-insert commentData
                insert_ $ EntryTree commentId entryId
                insertDefaultEntrySubscription commentId
                return commentId

            --rootEntry <- runDB $ get404 rootEntryId
            subscribers <- runDB $ selectList [EntrySubscriptionEntryId ==. entryId, EntrySubscriptionVerified ==. True] []
            forM_ subscribers $ \(Entity subscriptionId subscription) -> do
                
                let unsubscribeUrl= urlRenderParams (EditEntrySubscriptionR subscriptionId) $ case entrySubscriptionKey subscription of
                        Just key -> [("key", key)] 
                        Nothing -> []
                    entryUrl= urlRenderParams (UserEntryR rootEntryAuthorId rootEntryId) [] <> "#entry-" <> toPathPiece commentId
                sendAppEmail (entrySubscriptionEmail subscription) $ entrySubscriptionNotification unsubscribeUrl entryUrl commentData
                        

            setMessage $ [hamlet|
                            Your comment has been published. #
                            <a .alert-link.pull-right href=#entry-#{toPathPiece commentId}>View
                         |] urlRenderParams
            redirect $ UserEntryR rootEntryAuthorId rootEntryId :#: ("entry-" <> toPathPiece commentId)

        FormMissing -> do
            setMessageI MsgFormMissing
            redirect $ UserEntryR rootEntryAuthorId rootEntryId
        FormFailure _ -> do
            setMessageI MsgSomethingWrong
            redirect $ UserEntryR rootEntryAuthorId rootEntryId

deleteEntryRecursive :: EntryId -> ReaderT SqlBackend (HandlerFor App) ()
deleteEntryRecursive entryId = do
    children<-getChildIds entryId 
    deleteWhere [EntrySubscriptionEntryId <-. children++[entryId]]
    deleteWhere [EntryTreeNode <-. children++[entryId]]
    deleteWhere [EntryId <-. children++[entryId]]
    return ()
    
getChildIds :: EntryId -> ReaderT SqlBackend (HandlerFor App) [EntryId]
getChildIds entryId = do
    tree<-selectList [EntryTreeParent==.entryId] []
    case tree of
        [] -> return []
        x -> do
            let childIds = entryTreeNode . entityVal <$> x
            childIds'<-mapM getChildIds childIds
            return $ childIds ++ (concat childIds')

commentLink :: EntryId -> ReaderT SqlBackend (HandlerFor App) (Text, Text) -- (url,text)
commentLink commentId = do
    urlRender <- getUrlRender
    rootEntryId <- getRootEntryId commentId
    rootEntry <- get404 rootEntryId
    let rootEntryAuthorId = entryUserId rootEntry

    mTree <- selectFirst [EntryTreeNode==.commentId] []
    let parentId = case mTree of
            Nothing -> commentId
            Just tree -> entryTreeParent $ entityVal tree
    parent <- get404 parentId
    parentAuthor <- get404 $ entryUserId parent
    let url = urlRender (UserEntryR rootEntryAuthorId rootEntryId) <> "#entry-" <> toPathPiece commentId
    let text = case entryType parent of
            Comment -> "Reply to " <> userName parentAuthor <> "'s comment"
            _ -> "Comment on " <> userName parentAuthor <> "'s post"
    return (url, text)
