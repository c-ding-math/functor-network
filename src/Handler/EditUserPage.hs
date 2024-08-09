
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.EditUserPage where

import Import
--import Yesod.Form.Bootstrap3
import Handler.Parser(editorWidget,userTemporaryDirectory)
import Handler.EditUserEntry(EntryInput(..),entryInputForm,entry2Html)
--import Parse.Parser (mdToHtml,texToHtml,EditorData(..))
import Text.Shakespeare.Text
--import qualified Data.Text as T
--import System.Directory

getEditUserPageR :: Text -> Handler Html
getEditUserPageR title = do
    (userId, user) <- requireAuthPair
    case title of  
        "About" -> do
            mEntry<-runDB $ selectFirst [EntryType==.UserPage, EntryUserId==.userId] [Desc EntryInserted]
            
            formatParam <- lookupGetParam "format"
            let format = case (formatParam,mEntry) of
                    (Just "tex",_) -> Format "tex"
                    (Just "md",_) -> Format "md"
                    (_,Just entry) -> entryFormat $ entityVal entry
                    _ -> userDefaultFormat user

            (entryWidget, entryEnctype) <- case entityVal <$> mEntry of  
                Just entry-> do
                    generateFormPost $ entryInputForm $ Just $ EntryInput (entryTitle entry) format (entryPreamble entry) (entryBody entry) (entryCitation entry)
                Nothing->do 
                                    
                    urlRender<-getUrlRender
                    let defaultAboutInput = case format of 
                            Format "tex" -> Textarea ([st|
\section*{Basic Information}
\begin{itemize}
    \item \textbf{Name}: #{userName user}
    \item \textbf{Homepage}: \href{#{urlRender (UserHomeR userId)}}{#{urlRender (UserHomeR userId)}}
\end{itemize}
|]::Text)
                            _ -> Textarea ([st|
![](#{urlRender $ StaticR $ StaticRoute ["icons","user-photo.png"] []} "Avatar"){.float-right height=6em}

### Basic Information

- **Name**: #{userName user}
- **Homepage**: [#{urlRender (UserHomeR userId)}](#{urlRender (UserHomeR userId)})
|]::Text)
                    generateFormPost $ entryInputForm $ Just $ EntryInput "About" format (userDefaultPreamble user) (Just defaultAboutInput) (userDefaultCitation user)
            defaultLayout $ do
                setTitleI MsgEdit
                [whamlet|
        <form  method=post enctype=#{entryEnctype}>
            ^{entryWidget}
            <div .text-left>
                <button .btn .btn-primary type=submit name=action value=publish>_{MsgUpdate}
                <button .btn .btn-default .delete type=submit name=action value=delete>_{MsgReset}
                |]            
                toWidget        
                    [julius|
                        $(document).ready(function(){
                            $('.btn.delete').click(function(){
                                confirm("You are about to reset your about page! Are you really sure?");
                            });
                        });
                    |]
                
                editorWidget format
        _ -> notFound


postEditUserPageR :: Text -> Handler Html
postEditUserPageR title = do
    userId <- requireAuthId
    case title of
        "About" -> do
            mEntry<-runDB $ selectFirst [EntryType==.UserPage, EntryUserId==.userId] [Desc EntryInserted]
            ((res, _), _) <- runFormPost $ entryInputForm Nothing
            case res of 
                FormSuccess formData->  do
                    (titleHtml,bodyHtml)<-entry2Html formData
                    currentTime <- liftIO getCurrentTime
                    entryAction <- lookupPostParam "action"        
                    urlRenderParams<- getUrlRenderParams
                    case entryAction of
                        Just "delete"->case mEntry of 
                            Just (Entity entryId _)->do
                                runDB $ delete entryId 
                                setMessageI MsgAboutPageReset
                                redirect $ EditUserPageR title
                            Nothing->do
                                setMessageI MsgAboutPageReset
                                redirect $ EditUserPageR title
                        Just "publish"-> case mEntry of 
                            Nothing -> do
                                _<-runDB $ insert $ Entry
                                        { entryUserId=userId
                                        , entryType=UserPage
                                        , entryStatus=Publish
                                        , entryInserted=currentTime
                                        , entryUpdated=currentTime
                                        , entryTitle=inputTitle formData
                                        , entryPreamble=inputPreamble formData
                                        , entryFormat=inputFormat formData
                                        , entryBody=inputBody formData
                                        , entryCitation=inputCitation formData
                                        , entryTitleHtml=titleHtml
                                        , entryBodyHtml=bodyHtml
                                        }

                                setMessage $ [hamlet|
                                                Your page, #
                                                <a .alert-link href=@{UserPageR userId title}>#{title}
                                                , has been updated. #
                                                <a .alert-link.pull-right href=@{UserPageR userId title}>View
                                             |] urlRenderParams
                                   
                                redirect $ EditUserPageR title
                            Just (Entity entryId _) -> do
                                runDB $ update entryId
                                    [EntryStatus=.Publish
                                    ,EntryUpdated=.currentTime
                                    ,EntryTitle=.inputTitle formData
                                    ,EntryPreamble=.inputPreamble formData
                                    ,EntryFormat=.inputFormat formData
                                    ,EntryBody=.inputBody formData
                                    ,EntryCitation=.inputCitation formData
                                    ,EntryTitleHtml=.titleHtml
                                    ,EntryBodyHtml=.bodyHtml
                                    ] 

                                setMessage $ [hamlet|
                                                Your page, #
                                                <a .alert-link href=@{UserPageR userId title}>#{title}
                                                , has been updated. #
                                                <a .alert-link.pull-right href=@{UserPageR userId title}>View
                                             |] urlRenderParams
                                
                                redirect $ EditUserPageR title

                        Just "draft"-> case mEntry of 
                                Nothing -> do
                                    _<-runDB $ insert $ Entry
                                            { entryUserId=userId
                                            , entryType=UserPage
                                            , entryStatus=Draft
                                            , entryInserted=currentTime
                                            , entryUpdated=currentTime
                                            , entryTitle=inputTitle formData
                                            , entryPreamble=inputPreamble formData
                                            , entryFormat=inputFormat formData
                                            , entryBody=inputBody formData
                                            , entryCitation=inputCitation formData
                                            , entryTitleHtml=titleHtml
                                            , entryBodyHtml=bodyHtml
                                            }

                                    setMessage $ [hamlet|
                                                    Your page, #
                                                    <a .alert-link href=@{UserPageR userId title}>#{title}
                                                    , has been saved. #
                                                    <a .alert-link.pull-right href=@{UserPageR userId title}>View
                                                 |] urlRenderParams
                                    
                                    redirect $ EditUserPageR title

                                Just (Entity entryId _) -> do
                                    runDB $ update entryId
                                        [EntryStatus=.Draft
                                        ,EntryUpdated=.currentTime
                                        ,EntryTitle=.inputTitle formData
                                        ,EntryPreamble=.inputPreamble formData
                                        ,EntryFormat=.inputFormat formData
                                        ,EntryBody=.inputBody formData
                                        ,EntryCitation=.inputCitation formData
                                        ,EntryTitleHtml=.titleHtml
                                        ,EntryBodyHtml=.bodyHtml
                                        ] 

                                    setMessage $ [hamlet|
                                                    Your page, #
                                                    <a .alert-link href=@{UserPageR userId title}>#{title}
                                                    , has been saved. #
                                                    <a .alert-link.pull-right href=@{UserPageR userId title}>View
                                                 |] urlRenderParams
                                    
                                    redirect $ EditUserPageR title

                        _-> do 
                            setMessageI  MsgSomethingWrong
                            redirect $ EditUserPageR title
                FormFailure errors -> do
                    setMessage [shamlet|
                        $forall error <- errors
                            <p>#{error}
                        |]
                    redirect $ EditUserPageR title
                FormMissing -> do
                    setMessageI MsgFormMissing
                    redirect $ EditUserPageR title

        _ -> notFound
