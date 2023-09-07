{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.EditComment where

import Yesod.Form.Bootstrap3
import Handler.Parser(parse,userTemporaryDirectory)
import Parse.Parser(mdToHtml,texToHtml,EditorData(..))
import Import

deleteEditCommentR :: EntryId -> Handler ()
deleteEditCommentR commentId = runDB $ do
    maybeUserId<-maybeAuthId
    comment <- get404 commentId
    rootEntryId <- getRootEntryId commentId
    mRootEntry <- get rootEntryId

    case (isAdministrator maybeUserId comment, mRootEntry) of
        (True, _) -> deleteEntryRecursive commentId
        (_, Just entry) | isAdministrator maybeUserId entry -> deleteEntryRecursive commentId
        _ -> permissionDeniedI MsgPermissionDenied

data CommentInput=CommentInput
    {preamble::Maybe Textarea
    ,inputFormat::Format
    ,content::Textarea
    ,citation::Maybe Textarea
    } 
newCommentForm :: Maybe CommentInput -> Form CommentInput
newCommentForm mCommentData =  renderBootstrap3 BootstrapBasicForm $ CommentInput
    <$> aopt textareaField preambleSettings (preamble <$> mCommentData)
    <*> areq (selectFieldList inputFormats) "Comment" (inputFormat <$> mCommentData)
    <*> areq textareaField editorSettings (content <$> mCommentData)
    <*> aopt textareaField citationSettings (citation <$> mCommentData)
    where   inputFormats = [("Markdown", Format "md"), ("LaTeX", Format "tex")]::[(Text, Format)] 
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
    userId <- requireAuthId
    (rootEntryId, rootEntryAuthorId, entry) <- runDB $ do
        rootEntryId <- getRootEntryId entryId
        entry <- get404 entryId
        mRootEntryAuthorId <- entryUserId <$> get404 rootEntryId  
        case mRootEntryAuthorId of
            Just rootEntryAuthorId -> return (rootEntryId, rootEntryAuthorId, entry)
            Nothing -> notFound        

    urlRenderParams <- getUrlRenderParams
    
    ((res, _), _) <- runFormPost $ newCommentForm $ Nothing
    case res of
        FormSuccess newCommentFormData -> do
            let editorData=EditorData{
                editorPreamble=preamble newCommentFormData
                ,editorContent=Just (content newCommentFormData)
                ,editorCitation=citation newCommentFormData
            }
            currentTime <- liftIO getCurrentTime
            let parser=  case inputFormat newCommentFormData of
                        Format "tex" -> texToHtml
                        _ -> mdToHtml
            userDir<-userTemporaryDirectory
            bodyHtml <- liftIO $ parse userDir parser editorData
            let commentData=Entry 
                        {entryParentId= Just entryId
                        ,entryUserId=Just userId
                        ,entryType=Comment
                        ,entryInputFormat=inputFormat newCommentFormData
                        ,entryOutputFormat=Format "html"
                        ,entryInputTitle="Comment on "<> (entryInputTitle entry)
                        ,entryOutputTitle="Comment on "<> (entryOutputTitle entry)
                        ,entryInputPreamble=(preamble newCommentFormData)
                        ,entryInputBody=(content newCommentFormData)
                        ,entryOutputBody=bodyHtml
                        ,entryInputCitation=(citation newCommentFormData)
                        ,entryInserted=currentTime
                        ,entryUpdated=currentTime
                        --,entryStuck=Just currentTime
                        ,entryStatus=Publish
                        ,entryLocked=False
                        ,entryInputTags=[]
                        ,entryOutputTags=[]
                        }
                
            commentId <- runDB $ insert commentData
            
            setMessage $ [hamlet|<a href=#entry-#{toPathPiece commentId}>Your comment</a> has been published.|] urlRenderParams
            redirect $ EntryR rootEntryAuthorId rootEntryId :#: ("entry-" <> toPathPiece commentId)

        FormMissing -> do
            setMessageI MsgFormMissing
            redirect $ EntryR rootEntryAuthorId rootEntryId
        FormFailure _ -> do
            setMessageI MsgSomethingWrong
            redirect $ EntryR rootEntryAuthorId rootEntryId   

deleteEntryRecursive :: EntryId -> ReaderT SqlBackend (HandlerFor App) ()
deleteEntryRecursive entryId = do
    children<-getChildIds entryId
    mapM_ deleteEntryRecursive children
    delete entryId
    
getChildIds :: EntryId -> ReaderT SqlBackend (HandlerFor App) [EntryId]
getChildIds entryId = do
    children<-selectList [EntryParentId==.Just entryId] [Asc EntryInserted]
    let childIds=entityKey <$> children
    if null childIds then return [] else do
        childIds'<-mapM getChildIds childIds
        return $ childIds ++ (concat childIds')

getRootEntryId :: EntryId -> ReaderT SqlBackend (HandlerFor App) EntryId
getRootEntryId entryId = do
    entry<-get404 entryId
    case entryParentId entry of
        Just parentId -> getRootEntryId parentId
        Nothing -> return entryId
