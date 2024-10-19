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
import Handler.EditComment(deleteEditCommentR)

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
                          <ul .list-inline>
                            <li .by>
                                <span title="author"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-person" viewBox="0 0 16 16"><path d="M8 8a3 3 0 1 0 0-6 3 3 0 0 0 0 6m2-3a2 2 0 1 1-4 0 2 2 0 0 1 4 0m4 8c0 1-1 1-1 1H3s-1 0-1-1 1-4 6-4 6 3 6 4m-1-.004c-.001-.246-.154-.986-.832-1.664C11.516 10.68 10.289 10 8 10s-3.516.68-4.168 1.332c-.678.678-.83 1.418-.832 1.664z"/></svg><!-- Copyright (c) 2011-2024 The Bootstrap Authors, Licensed under MIT (https://github.com/twbs/bootstrap/blob/main/LICENSE) --> 
                                <a href=@{UserHomeR commentAuthorId}>#{commentAuthorName}
                            $maybe (parentCommentId, parentCommentAuthorName)<-mParentCommentMeta
                                <li .to>
                                    <span title="in reply to"><svg class="bi bi-reply" width="1em" height="1em" viewBox="0 0 16 16" fill="currentColor" xmlns="http://www.w3.org/2000/svg"><path fill-rule="evenodd" d="M9.502 5.013a.144.144 0 0 0-.202.134V6.3a.5.5 0 0 1-.5.5c-.667 0-2.013.005-3.3.822-.984.624-1.99 1.76-2.595 3.876C3.925 10.515 5.09 9.982 6.11 9.7a8.741 8.741 0 0 1 1.921-.306 7.403 7.403 0 0 1 .798.008h.013l.005.001h.001L8.8 9.9l.05-.498a.5.5 0 0 1 .45.498v1.153c0 .108.11.176.202.134l3.984-2.933a.494.494 0 0 1 .042-.028.147.147 0 0 0 0-.252.494.494 0 0 1-.042-.028L9.502 5.013zM8.3 10.386a7.745 7.745 0 0 0-1.923.277c-1.326.368-2.896 1.201-3.94 3.08a.5.5 0 0 1-.933-.305c.464-3.71 1.886-5.662 3.46-6.66 1.245-.79 2.527-.942 3.336-.971v-.66a1.144 1.144 0 0 1 1.767-.96l3.994 2.94a1.147 1.147 0 0 1 0 1.946l-3.994 2.94a1.144 1.144 0 0 1-1.767-.96v-.667z"/></svg><!-- Copyright (c) 2011-2024 The Bootstrap Authors, Licensed under MIT (https://github.com/twbs/bootstrap/blob/main/LICENSE) -->
                                    <a href=#entry-#{toPathPiece parentCommentId}>#{parentCommentAuthorName}
                            <li .at>
                                <span title="date"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-clock" viewBox="0 0 16 16"><path d="M8 3.5a.5.5 0 0 0-1 0V9a.5.5 0 0 0 .252.434l3.5 2a.5.5 0 0 0 .496-.868L8 8.71z"/><path d="M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16m7-8A7 7 0 1 1 1 8a7 7 0 0 1 14 0"/></svg><!-- Copyright (c) 2011-2024 The Bootstrap Authors, Licensed under MIT (https://github.com/twbs/bootstrap/blob/main/LICENSE) -->
                                #{utcToDate (entryInserted comment)}

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
                        ,entryFeatured=False
                        }
                
            commentId <- runDB $ do
                _ <- get404 entryId
                commentId<-insert commentData
                insert_ $ EntryTree commentId entryId currentTime
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