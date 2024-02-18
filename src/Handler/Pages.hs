{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Pages where

import Import
import Handler.UserEntries (entryListWidget)

getPagesR :: Handler Html
getPagesR = do
    entryList<-runDB $ do     
        entries<- selectList [EntryType==.Page] [Desc EntryInserted]
        return $ entries
    --(widget, enctype)<-generateFormPost preEntryForm
    defaultLayout $ do
        setTitleI MsgPages
        [whamlet|        
            <h1>_{MsgPages}
            $if null entryList
                <div> _{MsgNoPage}
            $else
                ^{entryListWidget entryList}
        |]



