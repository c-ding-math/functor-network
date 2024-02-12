{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Pages0 where

import Import
import Handler.Entries (entryListWidget)

getPages0R :: Handler Html
getPages0R = do
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



