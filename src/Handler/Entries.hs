{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Entries where

import Import
import Handler.Parser (tagsWidget)
import Parse.Parser (scaleHeader)
--import Yesod.Form.Bootstrap3

{-data PreEntry = PreEntry {
    preEntryInputFormat :: Format,
    preEntryOutputFormat :: Format
    } deriving Show

preEntryForm:: Form PreEntry
preEntryForm = renderBootstrap3 BootstrapBasicForm $ PreEntry
    <$> areq (radioFieldList inputFormats) (bfs MsgInputFormat) Nothing
    <*> areq (radioFieldList outputFormats) (bfs MsgOutputFormat) Nothing
    where
        inputFormats = [("Markdown", Format "md"), ("LaTeX", Format "tex")]::[(Text, Format)]
        outputFormats = [("Standard", Format "html"), ("Slides", Format "slides")]::[(Text, Format)]
        -}

getEntriesR :: Path -> Handler Html
getEntriesR piece = do
    mCurrentUserId<-maybeAuthId
    entryList<-runDB $ do
        _<-get404 piece      
        entries<- selectList [EntryUserId==.piece, EntryType==.Standard] [Desc EntryInserted]
        let entryList =[x | x<-entries, entryStatus (entityVal x) == Publish||isAdministrator mCurrentUserId (entityVal x)]          
        return $ entryList
    --(widget, enctype)<-generateFormPost preEntryForm
    defaultLayout $ do
        setTitleI MsgEntries
        [whamlet|        
            <h1>_{MsgEntries}
                $if mCurrentUserId == Just piece
                    <a .btn.btn-primary .new-entry.navbar-right href=@{NewEntryR}>_{MsgNewPost}
            $if null entryList
                <div> _{MsgNoPost}
            $else
                <div .entries>
                    <ul>
                        $forall Entity entryId entry<-entryList
                            <li :entryStatus entry == Draft:.draft>
                                <a href=@{EntryR piece entryId}>
                                    <h2>#{preEscapedToMarkup (scaleHeader 2 (entryOutputTitle entry))}
                                <div .tags>
                                    ^{tagsWidget piece (zip (entryInputTags entry) (entryOutputTags entry))}

        |]
            
        addStylesheet $ StaticR css_entry_list_css


