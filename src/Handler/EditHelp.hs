{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.EditHelp where

import Import
import Yesod.Form.Bootstrap3
import Handler.Parser(editorWidget)

data EntryInput=EntryInput
    { inputFormat::Format
    , preamble:: Maybe Textarea
    , body::Maybe Textarea 
    , citation:: Maybe Textarea
    } deriving Show

entryForm::Maybe EntryInput -> Html -> MForm Handler (FormResult EntryInput, Widget)
entryForm inputs=renderBootstrap3 BootstrapBasicForm $ EntryInput
    <$> areq (selectFieldList inputFormats) formatSettings (inputFormat <$> inputs)
    <*> aopt textareaField preambleSettings (preamble <$> inputs)
    <*> aopt textareaField editorSettings  (body  <$> inputs)
    <*> aopt textareaField citationSettings (citation  <$> inputs) where  
        inputFormats = [(MsgMarkdownWithLaTeX, Format "md"), (MsgPureLaTeX, Format "tex")]
        formatSettings =  FieldSettings
            { fsLabel = SomeMessage MsgFormat
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "format"
            , fsAttrs =[("class", "input-sm form-control format-selector")]}
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
                maybeSyntax<-runDB $ selectFirst [EntryTitle==."Latex Help",EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]
                maybeDemo <-runDB $ selectFirst [EntryTitle==."a sample post written in latex",EntryType==.Page,EntryStatus==.Publish] [Desc EntryInserted]
                return (maybeSyntax, maybeDemo, Format "tex", "Latex Help"::Text)
            _ -> do
                maybeSyntax<-runDB $ selectFirst [EntryTitle==."Markdown Help",EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]
                maybeDemo <-runDB $ selectFirst [EntryTitle==."a sample post written in markdown",EntryType==.Page,EntryStatus==.Publish] [Desc EntryInserted]
                return (maybeSyntax, maybeDemo, Format "md", "Markdown Help"::Text)
    entryWidget <- case maybeDemo of
        Just (Entity _ entry) -> do
            (entryWidget, _) <- generateFormPost $ entryForm $ Just $ EntryInput (entryFormat entry) (entryPreamble entry) (entryBody entry) (entryCitation entry) 
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
            #{preEscapedToMarkup(entryBodyHtml entry)}
        $nothing
            <p>_{MsgComingSoon}
<section .new-comment>
            <h3 #editor>Editor Demo
            <form .editor-demo>
                ^{entryWidget}  
        |]
        editorWidget format

getEditHelpR "editor" = do
    let title="Editor Help" :: Text
    formatParam <- lookupGetParam "format"
    (maybeSyntax, maybeDemo, format) <- case formatParam of
            Just "tex" -> do
                maybeSyntax<-runDB $ selectFirst [EntryTitle==."Latex Editor Help",EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]
                maybeDemo <-runDB $ selectFirst [EntryTitle==."a sample post written in latex",EntryType==.Page,EntryStatus==.Publish] [Desc EntryInserted]
                return (maybeSyntax, maybeDemo, Format "tex")
            _ -> do
                maybeSyntax<-runDB $ selectFirst [EntryTitle==."Markdown Editor Help",EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]
                maybeDemo <-runDB $ selectFirst [EntryTitle==."a sample post written in markdown",EntryType==.Page,EntryStatus==.Publish] [Desc EntryInserted]
                return (maybeSyntax, maybeDemo, Format "md")
    
    entryWidget <- case maybeDemo of
        Just (Entity _ entry) -> do
            (entryWidget, _) <- generateFormPost $ entryForm $ Just $ EntryInput (entryFormat entry) (entryPreamble entry) (entryBody entry) (entryCitation entry) 
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
            #{preEscapedToMarkup(entryBodyHtml entry)}
        $nothing
            <p>_{MsgComingSoon}
<section .new-comment>
            <h3 #editor>Editor Demo
            <form .editor-demo>
                ^{entryWidget}  
        |]
        editorWidget format

getEditHelpR "format" = do
    let title="Format Comparison" :: Text
    mFormat <- runDB $ selectFirst [EntryTitle==.title,EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]
    defaultLayout $ do
        setTitle "Edit Help"
        [whamlet|
            <article .entry>
                <h1>#{title}
                <div .entry-content>
                    <div .entry-content-wrapper>
                        $maybe (Entity _ entry)<-mFormat
                            #{preEscapedToMarkup (entryBodyHtml entry)}
                        $nothing     
                            <p>_{MsgComingSoon}
        |]

getEditHelpR "shortcuts" = do
    let title="Shortcuts" :: Text
    mShortcuts <- runDB $ selectFirst [EntryTitle==.title,EntryType==.Page,EntryStatus==.Draft] [Desc EntryInserted]
    defaultLayout $ do
        setTitle "Edit Help"
        [whamlet|
            <article .entry>
                <h1>#{title}
                <div .entry-content>
                    <div .entry-content-wrapper>
                        $maybe (Entity _ entry)<-mShortcuts
                            #{preEscapedToMarkup (entryBodyHtml entry)}
                        $nothing     
                            <p>_{MsgComingSoon}
        |]

getEditHelpR _ = notFound