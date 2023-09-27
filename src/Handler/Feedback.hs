{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Feedback where

import Import
import Text.Shakespeare.Text
import Yesod.Form.Bootstrap3
--import Handler.Parser(markItUpWidget)

data Feedback = Feedback
    { subject :: Text
    , body :: Textarea
    }

feedbackForm :: Maybe Feedback ->  Html -> MForm Handler (FormResult Feedback, Widget)
feedbackForm feedback = renderBootstrap3 BootstrapBasicForm $ Feedback
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
    mFeedbackDescription<-runDB $ selectFirst [EntryInputTitle==."Feedback",EntryType==.Page0,EntryStatus==.Draft] [Desc EntryInserted]
    (feedbackWidget, feedbackEnctype) <- generateFormPost $ feedbackForm $ Just $ Feedback "Feedback" $ Textarea ""
    defaultLayout $ do
        setTitleI MsgFeedback
        [whamlet|
<div .entry>
  <h1>_{MsgFeedback}
  <div .entry-content>
    $maybe (Entity _ entry)<-mFeedbackDescription
        <article>#{preEscapedToMarkup(entryOutputBody entry)}
    $nothing
        _{MsgComingSoon}
<section .new-comment>
    <h3>_{MsgSendAFeedback}
    <form .feedback-form method=post action=@{FeedbackR} enctype=#{feedbackEnctype}>
        ^{feedbackWidget}
        <button type="submit" .btn.btn-primary>_{MsgSend}
        |]

postFeedbackR :: Handler Html
postFeedbackR = do
    --maybeUser <- maybeAuthPair

    ((result, _), _) <- runFormPost $ feedbackForm Nothing
    case result of
        FormSuccess feedback -> do
            master <- getYesod
            let systemEmailUser=appEmailUser $ appSettings master
                emailSubject = subject feedback
                emailText = [stext|#{unTextarea(body feedback)}|]
                emailHtml = [shamlet|
#{unTextarea(body feedback)}
|]
            sendSystemEmail systemEmailUser emailSubject emailText emailHtml
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