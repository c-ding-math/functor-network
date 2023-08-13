{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Pages0 where

import Import
import Parse.Parser (scaleHeader)

getPages0R :: Handler Html
getPages0R = do
    entryList<-runDB $ do     
        entries<- selectList [EntryType==.Page0] [Desc EntryInserted]
        return $ entries
    --(widget, enctype)<-generateFormPost preEntryForm
    defaultLayout $ do
        setTitleI MsgPages
        [whamlet|        
            <h1>_{MsgPages}
            $if null entryList
                <div> _{MsgNoPage}
            $else
                <div .entries>
                    <ul>
                        $forall Entity _ entry<-entryList
                            <li :entryStatus entry == Draft:.draft>
                                <a href=@{Page0R (entryInputTitle entry)}>
                                    <h2>#{preEscapedToMarkup (scaleHeader 2 (entryOutputTitle entry))}
                                <a .entry-menu href=@{EditPage0R (entryInputTitle entry)}>_{MsgEdit}
                                
        |]
            
        $(widgetFile "entry-list")


