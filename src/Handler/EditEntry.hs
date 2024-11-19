
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.EditEntry where

import Import
import Handler.EditComment(deleteEntryRecursive)
import Handler.Parser(editorWidget)
import Handler.EditUserEntry(EntryInput(..),entryInputForm,entry2Html)
import Database.Persist.Sql

getEditEntryR :: EntryId -> Handler Html
getEditEntryR entryId = do
    --userId <- requireAuthId
    entry<-runDB $ get404 entryId   
 
    formatParam <- lookupGetParam "format"
    let format = case formatParam of
            Just "tex" -> Format "tex"
            Just "md" -> Format "md"
            _ -> entryFormat entry
    (inputWidget, inputEnctype) <- generateFormPost $ entryInputForm $ Just $ EntryInput
        { inputTitle=entryTitle entry
        , inputFormat=format
        , inputPreamble=entryPreamble entry
        , inputBody=entryBody entry
        , inputCitation=entryCitation entry
        }

    defaultLayout $ do
        setTitleI MsgEdit
        [whamlet|
<a href=@{UserEntryR (entryUserId entry) entryId}>View
<form  method=post enctype=#{inputEnctype}>
    
    <div .text-left>
        <button .btn .btn-default .save type=submit name=action value=save>_{MsgSave}
        <button .btn .btn-default .delete type=submit name=action value=delete>_{MsgDelete}
        <a .btn .btn-default href=@{EditEntryR $ toSqlKey $ fromSqlKey entryId + 1}>Next
    ^{inputWidget}
        |]
            
        toWidget        
            [julius|
                $(document).ready(function(){
                    $('.btn.delete').click(function(){
                        if(!confirm("You are about to delete your entry! Are you really sure?")){
                            return false;
                        };
                    });
                    $('.btn.save').click(function(){
                        var title= $("input[name='title']").val();
                        if(title.length>256){
                            alert("Title is too long. Please make it shorter.");
                            return false;
                        }
                    });
                });
            |]
        
        editorWidget format

postEditEntryR :: EntryId -> Handler Html
postEditEntryR  entryId = do
    ((res, _), _) <- runFormPost $ entryInputForm Nothing
    case res of 
        FormSuccess formData->  do

            (titleHtml,bodyHtml) <- entry2Html formData
            --currentTime <- liftIO getCurrentTime
            urlRenderParams<- getUrlRenderParams
            entryAction <- lookupPostParam "action"  
            case entryAction of
                Just "delete"->  do
                    runDB $ deleteEntryRecursive entryId
                    setMessage $ [shamlet|The post, #{inputTitle formData}, has been deleted.|] --getUrlRenderParams
                    defaultLayout $ do
                        [whamlet|
                            <a href=@{EntriesR}>Entries page
                        |]
                _-> do 
                    entry<-runDB $ get404 entryId
                    let authorId=entryUserId entry
                    runDB  $ do                         
                        update entryId                    
                            [EntryTitle=.inputTitle formData
                            ,EntryPreamble=.inputPreamble formData
                            ,EntryFormat=.inputFormat formData
                            ,EntryBody=.inputBody formData
                            ,EntryCitation=.inputCitation formData
                            ,EntryTitleHtml=.titleHtml
                            ,EntryBodyHtml=.bodyHtml
                            ] 
                        
                    setMessage $ [hamlet|
                                    The post, #
                                    <a .alert-link href=@{UserEntryR authorId entryId}>#{inputTitle formData}
                                    , has been saved. #
                                    <a .alert-link.pull-right href=@{UserEntryR authorId entryId}>View
                                 |] urlRenderParams
                    redirect $ EditEntryR entryId

        FormMissing -> do          
            setMessageI MsgFormMissing
            redirect $ EditEntryR entryId
        FormFailure errors -> do
            setMessage [shamlet|
                $forall error <- errors
                    <p>#{error}
                |]
            redirect $ EditEntryR entryId
