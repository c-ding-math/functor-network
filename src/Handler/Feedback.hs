{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Feedback where

import Import
import Text.Shakespeare.Text
import Yesod.Form.Bootstrap3
import Handler.EditComment(getChildIds,CommentInput(..))
import Handler.UserEntry(menuWidget)
import Parse.Parser(mdToHtml,texToHtml,EditorData(..))
import Handler.Parser(editorWidget)
import Handler.Parser(parse,userTemporaryDirectory)
import Handler.NewEntrySubscription(entrySubscriptionNotification,insertDefaultEntrySubscription)
import Handler.EditComment(deleteEntryRecursive,deleteEditCommentR)

data FeedbackInput = FeedbackInput
    { subject :: Text
    , content :: Textarea
    }

feedbackForm :: Maybe FeedbackInput ->  Form FeedbackInput
feedbackForm feedback = renderBootstrap3 BootstrapBasicForm $ FeedbackInput
    <$> areq textField subjectSetting (subject <$> feedback)
    <*> areq textareaField editorSettings (content <$> feedback) where  
        subjectSetting = FieldSettings
            { fsLabel = "Subject"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "subject"
            , fsAttrs =[ ("class", "editor form-control"), ("placeholder", "Your subject")]}
        editorSettings = FieldSettings
            { fsLabel = "Content"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "content"
            , fsAttrs =[ ("class", "editor form-control"), ("placeholder", "Your content goes here.")]}
            
newCommentForm :: Maybe CommentInput -> Form CommentInput
newCommentForm mCommentData =  renderBootstrap3 BootstrapBasicForm $ CommentInput
    <$> aopt textareaField preambleSettings (preamble <$> mCommentData)
    <*> areq (selectFieldList inputFormats) formatSettings (inputFormat <$> mCommentData)
    <*> areq textareaField editorSettings (body <$> mCommentData)
    <*> aopt textareaField citationSettings (citation <$> mCommentData)
    where   
            inputFormats = [(MsgMarkdownWithLaTeX, Format "md"), (MsgPureLaTeX, Format "tex")]
            formatSettings =  FieldSettings
                { fsLabel = SomeMessage MsgFeedback
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

getFeedbackR :: Handler Html
getFeedbackR = do
    mFeedbackDescription<-runDB $ selectFirst [EntryTitle==."Feedback",EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]
    case mFeedbackDescription of
        Nothing -> notFound
        Just (Entity entryId entry) -> do
            maybeUserId <- maybeAuthId
            
            commentListData <- case maybeUserId of
                Just userId -> runDB $ do
                    commentEntities <- if isAdministrator (Just userId) entry
                        then selectList [EntryType==.Feedback,EntryStatus==.Draft] [Asc EntryInserted]
                        else do    
                            userComments <- selectList [EntryUserId==.userId,EntryType==.Feedback,EntryStatus==.Draft] []
                            childCommentIds <- mapM getChildIds $ map entityKey userComments
                            let commentIds = map entityKey userComments ++ concat childCommentIds
                            selectList [EntryId <-. commentIds] [Asc EntryInserted]
                            
                    commentAuthors <- mapM (\(Entity _ comment) -> do
                        commentAuthor<-get404 $ entryUserId comment
                        return (entryUserId comment, userName commentAuthor)
                        ) commentEntities
                    parentCommentMetas <- mapM (\commentEntity -> do
                        mEntryTree <- selectFirst [EntryTreeNode==.entityKey commentEntity] []
                        case mEntryTree of
                            Just tree -> do
                                let parentCommentId = entryTreeParent $ entityVal tree
                                if parentCommentId == entryId
                                    then return Nothing
                                    else do
                                        parentComment <- get404 parentCommentId
                                        parentCommentAuthor <- get404 $ entryUserId parentComment
                                        return $ Just (parentCommentId, userName parentCommentAuthor)
                            _ -> return Nothing
                        ) commentEntities
                    return $ zip3 commentEntities commentAuthors parentCommentMetas
                _ -> return []
            formatParam <- lookupGetParam "format"
            maybeUser <- maybeAuth
            let format = case formatParam of
                    Just "tex" -> Format "tex"
                    Just "md" -> Format "md"
                    _ -> case maybeUser of
                            Just (Entity _ user) -> userDefaultFormat user
                            Nothing -> Format "md"
            (commentWidget, commentEnctype) <- case maybeUser of
                Nothing ->  generateFormPost $ feedbackForm $ Just $ FeedbackInput "Feedback" $ Textarea ""
                Just (Entity _ user) -> generateFormPost $ newCommentForm $ Just $ CommentInput (userDefaultPreamble user) format (Textarea "") (userDefaultCitation user)

            defaultLayout $ do
                setTitleI MsgFeedback
                [whamlet|
        <article .entry>
            <h1>_{MsgFeedback}
            <div .entry-content>
                $maybe (Entity _ entry)<-mFeedbackDescription
                    <div .entry-content-wrapper>#{preEscapedToMarkup(entryBodyHtml entry)}
                $nothing
                            _{MsgComingSoon}

            <div .menu.hidden>
                <ul.list-inline.text-lowercase>
                    <li .reply>
                        <a.text-muted href=#comment data-action=@{EditFeedbackR entryId}>_{MsgComment}
                    <!--<li .print><a.text-muted href=#>print</a>-->
                    <li .subscribe>
                        <a.text-muted href=# data-action=@{NewEntrySubscriptionR entryId}>_{MsgFollow}     
                    $if isAdministrator maybeUserId entry
                        <li .edit>
                            <a.text-muted href=@{EditUserEntryR entryId}>_{MsgEdit}
        
        <section #comments .comments  :entryStatus entry == Draft:.draft>    
            $if null commentListData
                <p style="display:none">_{MsgNoComment}
            $else
                <h3>_{MsgYourFeedback}
                $forall (Entity commentId comment,(commentAuthorId, commentAuthorName),mParentCommentMeta)<-commentListData
                    <article .comment id=entry-#{toPathPiece commentId}> 
                        <div .entry-meta>
                            <span .by>                  
                                <a href=@{UserHomeR commentAuthorId}>#{commentAuthorName}
                            $maybe (parentCommentId, parentCommentAuthorName)<-mParentCommentMeta
                                <span .to>
                                    <svg style="height:1em;vertical-align:middle;" class="bi bi-reply-fill" width="1em" height="1em" viewBox="0 0 16 16" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
                                        <path d="M9.079 11.9l4.568-3.281a.719.719 0 0 0 0-1.238L9.079 4.1A.716.716 0 0 0 8 4.719V6c-1.5 0-6 0-7 8 2.5-4.5 7-4 7-4v1.281c0 .56.606.898 1.079.62z"/>
                                    <a href=#entry-#{toPathPiece parentCommentId}>
                                        #{parentCommentAuthorName}
                            <span .at>#{utcToDate (entryInserted comment)}

                        <div .entry-content>
                            <div .entry-content-wrapper>#{preEscapedToMarkup (entryBodyHtml comment)}  
                        <div.menu>
                            <ul.list-inline.text-lowercase>                
                                <li .reply>
                                    <a.text-muted href=#comment data-action=@{EditFeedbackR commentId}>reply
                                <li .subscribe>
                                    <a.text-muted href=# data-action=@{NewEntrySubscriptionR commentId}>_{MsgFollow}
                                $if isAdministrator maybeUserId entry || isAdministrator maybeUserId comment
                                    <li .delete>
                                        <a.text-muted href=@{EditFeedbackR commentId}>_{MsgDelete}
        <section .new-comment>
            <h3 #comment>_{MsgSendFeedback}
            <form method=post enctype=#{commentEnctype}>
                ^{commentWidget}
                <div .text-left>    
                    <button type="submit" .btn.btn-primary>_{MsgSend}

                |]

                if (isJust maybeUserId) 
                    then do
                        editorWidget format
                        menuWidget
                    else 
                        toWidget [julius|
                            //$(document).ready(function(){
                                // Spam prevention
                                var t = true;
                                setTimeout(function(){
                                    t=false;
                                }, 1000);
                            //});
                                $("form").submit(function(){
                                    if(t){
                                        //alert("Please wait a moment before submitting the form.");
                                        return false;
                                    }
                                });
                            |]
        
postFeedbackR :: Handler Html
postFeedbackR = do
    mFeedbackEntry <- runDB $  selectFirst [EntryTitle==."Feedback",EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]
    case mFeedbackEntry of
        Nothing -> notFound
        Just (Entity entryId _) -> do
            maybeUserId <- maybeAuthId
            case maybeUserId of
                Nothing -> do
                    ((result, _), _) <- runFormPost $ feedbackForm Nothing
                    case result of
                        FormSuccess feedback -> do
                            
                            let feedbackEmailAddress ="feedback@functor.network"
                                emailSubject = subject feedback
                                emailText = [stext|#{unTextarea(content feedback)}|]
                                emailHtml = [shamlet|
                                    #{unTextarea(content feedback)}
                                    |]
                            sendAppEmail feedbackEmailAddress $ AppEmail emailSubject emailText emailHtml
                            defaultLayout $ do
                                setTitleI MsgFeedback
                                [whamlet|
                                    <h1>_{MsgFeedback}
                                    <p>_{MsgFeedbackSentDescription}
                                |]
                        FormMissing -> do
                            defaultLayout $ do
                                setTitleI MsgFeedback
                                [whamlet|
                                    <p>_{MsgFormMissing}
                                |]
                        FormFailure errors -> do
                            defaultLayout $ do
                                setTitleI MsgFeedback
                                [whamlet|
                                    $forall error <- errors
                                        <p>#{error}
                                |]

                Just _ -> postEditFeedbackR entryId


postEditFeedbackR :: EntryId -> Handler Html
postEditFeedbackR entryId = do
    
    Entity userId _ <- requireAuth
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
            let title = "Feedback"
                
            bodyHtml <- liftIO $ parse userDir parser editorData
            let commentData=Entry 
                        {entryUserId=userId
                        ,entryType=Feedback
                        ,entryFormat=inputFormat newCommentFormData
                        ,entryTitle=title
                        ,entryTitleHtml=title
                        ,entryPreamble=(preamble newCommentFormData)
                        ,entryBody=Just $ body newCommentFormData
                        ,entryBodyHtml=bodyHtml
                        ,entryCitation=citation newCommentFormData
                        ,entryInserted=currentTime
                        ,entryUpdated=currentTime
                        ,entryStatus=Draft
                        }
                
            commentId <- runDB $ do
                _ <- get404 entryId
                commentId<-insert commentData
                insert_ $ EntryTree commentId entryId
                insertDefaultEntrySubscription commentId
                return commentId

            subscribers <- runDB $ selectList [EntrySubscriptionEntryId ==. entryId, EntrySubscriptionVerified ==. True] []
            forM_ subscribers $ \(Entity subscriptionId subscription) -> do
                
                let unsubscribeUrl= urlRenderParams (EditEntrySubscriptionR subscriptionId) $ case entrySubscriptionKey subscription of
                        Just key -> [("key", key)] 
                        Nothing -> []
                    entryUrl= urlRenderParams (FeedbackR) [] <> "#entry-" <> toPathPiece commentId
                sendAppEmail (entrySubscriptionEmail subscription) $ entrySubscriptionNotification unsubscribeUrl entryUrl commentData
                        

            setMessage $ [hamlet|
                            Your feedback has been saved. #
                            <a .alert-link.pull-right href=#entry-#{toPathPiece commentId}>View
                         |] urlRenderParams
            redirect $ FeedbackR :#: ("entry-" <> toPathPiece commentId)

        FormMissing -> do
            setMessageI MsgFormMissing
            redirect $ FeedbackR
        FormFailure _ -> do
            setMessageI MsgSomethingWrong
            redirect $ FeedbackR

deleteEditFeedbackR :: EntryId -> Handler ()
deleteEditFeedbackR = deleteEditCommentR