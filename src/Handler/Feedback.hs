{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Feedback where

import Import
import Text.Shakespeare.Text
import Yesod.Form.Bootstrap3
--import Handler.Parser(markItUpWidget)

data FeedbackInput = FeedbackInput
    { subject :: Text
    , body :: Textarea
    }

feedbackForm :: Maybe FeedbackInput ->  Form FeedbackInput
feedbackForm feedback = renderBootstrap3 BootstrapBasicForm $ FeedbackInput
    <$> areq textField subjectSetting (subject <$> feedback)
    <*> areq textareaField editorSettings (body <$> feedback) where  
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
            

getFeedbackR :: Handler Html
getFeedbackR = do
    mFeedbackDescription<-runDB $ selectFirst [EntryTitle==."Feedback",EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]
    (feedbackWidget, feedbackEnctype) <- generateFormPost $ feedbackForm $ Just $ FeedbackInput "Feedback" $ Textarea ""
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
<section .new-comment>
    <h3>_{MsgSendFeedback}
    <form .feedback-form method=post action=@{FeedbackR} enctype=#{feedbackEnctype}>
        ^{feedbackWidget}
        <div .text-left>    
            <button type="submit" .btn.btn-primary>_{MsgSend}
        |]
        toWidget [julius|
        //$(document).ready(function(){
            // Spam prevention
            var t = true;
            setTimeout(function(){
                t=false;
            }, 100);
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
    --maybeUser <- maybeAuthPair

    ((result, _), _) <- runFormPost $ feedbackForm Nothing
    case result of
        FormSuccess feedback -> do
            
            let feedbackEmailAddress ="feedback@functor.network"
                emailSubject = subject feedback
                emailText = [stext|#{unTextarea(body feedback)}|]
                emailHtml = [shamlet|
#{unTextarea(body feedback)}
|]

            sendAppEmail feedbackEmailAddress $ AppEmail emailSubject emailText emailHtml
            maybeUserId <- maybeAuthId
            case maybeUserId of
                Just userId -> do
                    currentTime <- liftIO getCurrentTime
                    runDB $ insert_ $ Entry
                            { entryUserId=userId
                            , entryType=Feedback
                            , entryStatus=Draft
                            , entryInserted=currentTime
                            , entryUpdated=currentTime
                            , entryTitle=subject feedback
                            , entryPreamble=Nothing
                            , entryFormat=Format "md"
                            , entryBody=Just (body feedback)
                            , entryCitation=Nothing
                            , entryTitleHtml=subject feedback
                            , entryBodyHtml=unTextarea (body feedback)
                            }
                    --insert_ $ EntryTree commentId entryId
                    return ()
                Nothing -> return ()
            
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