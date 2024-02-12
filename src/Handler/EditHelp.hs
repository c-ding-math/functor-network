{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.EditHelp where

import Import
import Yesod.Form.Bootstrap3
import Handler.Parser(markItUpWidget)

data EntryInput=EntryInput
    { preamble:: Maybe Textarea
    , inputFormat::Format
    , content::Textarea 
    , citation:: Maybe Textarea
    } deriving Show

entryForm::Maybe EntryInput -> Html -> MForm Handler (FormResult EntryInput, Widget)
entryForm inputs=renderBootstrap3 BootstrapBasicForm $ EntryInput
    <$> aopt textareaField preambleSettings (preamble <$> inputs)
    <*> areq (selectFieldList inputFormats) "Format" (inputFormat <$> inputs)
    <*> areq textareaField editorSettings  (content  <$> inputs)
    <*> aopt textareaField citationSettings (citation  <$> inputs) where  
        inputFormats = [("Markdown", Format "md"), ("LaTeX", Format "tex")]::[(Text, Format)] 
        editorSettings = FieldSettings
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "content"
            , fsAttrs =[ ("class", "editor form-control"), ("placeholder", "Your content goes here. Press [Ctrl + 0] to preview.")]}
        preambleSettings = FieldSettings
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "preamble"
            , fsAttrs =[("class", "hidden")]}
        citationSettings = FieldSettings
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "citation"
            , fsAttrs =[("class", "hidden")]}


getEditHelpR :: Text -> Handler Html
getEditHelpR "syntax" = do
    formatParam <- lookupGetParam "format"
    (maybeSyntax, maybeDemo, format, title) <- case formatParam of
            Just "tex" -> do
                maybeSyntax<-runDB $ selectFirst [EntryInputTitle==."Latex Help",EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]
                maybeDemo <-runDB $ selectFirst [EntryInputTitle==."a sample post written in latex",EntryType==.Page,EntryStatus==.Publish] [Desc EntryInserted]
                return (maybeSyntax, maybeDemo, Format "tex", "Latex Help"::Text)
            _ -> do
                maybeSyntax<-runDB $ selectFirst [EntryInputTitle==."Markdown Help",EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]
                maybeDemo <-runDB $ selectFirst [EntryInputTitle==."a sample post written in markdown",EntryType==.Page,EntryStatus==.Publish] [Desc EntryInserted]
                return (maybeSyntax, maybeDemo, Format "md", "Markdown Help"::Text)
    entryWidget <- case maybeDemo of
        Just (Entity _ entry) -> do
            (entryWidget, _) <- generateFormPost $ entryForm $ Just $ EntryInput (entryInputPreamble entry) (entryInputFormat entry) (entryInputBody entry) (entryInputCitation entry) 
            return entryWidget
        Nothing -> do
            (entryWidget, _) <- generateFormPost $ entryForm Nothing
            return entryWidget
    defaultLayout $ do
        setTitle "Edit Help"
        [whamlet|
<article .entry>
  <h1>#{title}
  <div .entry-content>
    <div .entry-content-wrapper>
        $maybe (Entity _ entry)<-maybeSyntax
            #{preEscapedToMarkup(entryOutputBody entry)}
        $nothing
            <p>_{MsgComingSoon}
<section .new-comment>
            <h3 #editor>Editor Demo
            <form .editor-demo>
                ^{entryWidget}  
        |]
        markItUpWidget format (Format "html")

getEditHelpR "editor" = do
    let title="Editor Help" :: Text
    formatParam <- lookupGetParam "format"
    (maybeSyntax, maybeDemo, format) <- case formatParam of
            Just "tex" -> do
                maybeSyntax<-runDB $ selectFirst [EntryInputTitle==."Latex Editor Help",EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]
                maybeDemo <-runDB $ selectFirst [EntryInputTitle==."a sample post written in latex",EntryType==.Page,EntryStatus==.Publish] [Desc EntryInserted]
                return (maybeSyntax, maybeDemo, Format "tex")
            _ -> do
                maybeSyntax<-runDB $ selectFirst [EntryInputTitle==."Markdown Editor Help",EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]
                maybeDemo <-runDB $ selectFirst [EntryInputTitle==."a sample post written in markdown",EntryType==.Page,EntryStatus==.Publish] [Desc EntryInserted]
                return (maybeSyntax, maybeDemo, Format "md")
    
    entryWidget <- case maybeDemo of
        Just (Entity _ entry) -> do
            (entryWidget, _) <- generateFormPost $ entryForm $ Just $ EntryInput (entryInputPreamble entry) (entryInputFormat entry) (entryInputBody entry) (entryInputCitation entry) 
            return entryWidget
        Nothing -> do
            (entryWidget, _) <- generateFormPost $ entryForm Nothing
            return entryWidget
    defaultLayout $ do
        setTitle "Edit Help"
        [whamlet|
<article .entry>
  <h1>#{title}
  <div .entry-content>
    <div .entry-content-wrapper>
        $maybe (Entity _ entry)<-maybeSyntax
            #{preEscapedToMarkup(entryOutputBody entry)}
        $nothing
            <p>_{MsgComingSoon}
<section .new-comment>
            <h3 #editor>Editor Demo
            <form .editor-demo>
                ^{entryWidget}  
        |]
        markItUpWidget format (Format "html")

getEditHelpR _ = notFound