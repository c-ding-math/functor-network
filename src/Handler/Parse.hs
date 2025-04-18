{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Parse where

import Import

import System.Directory
import System.FilePath
import System.Random
import Parse.Parser(parse, downloadPdfFileName, preProcessEditorData, texToPdf, texToHtml, texToHtmlSimple, mdToPdf, mdToHtml, mdToHtmlSimple, texToSvg, EditorData(..))
--import Data.Text.IO
--import qualified Data.Text as T
--import Codec.Archive.Zip
--import qualified Prelude
--import System.Environment (getExecutablePath)

type InputFormat=Text
type OutputFormat=Text

postParseR :: InputFormat->OutputFormat-> Handler RepPlain
postParseR inputFormat outputFormat = do
    userDir<-userTemporaryDirectory
    subdirectoriesNumber<-liftIO $ countActiveSubdirectories $ takeDirectory userDir
    if subdirectoriesNumber > 0 then do
        let busyMessage::Text
            busyMessage= "Busy..."
        return $ RepPlain $ toContent $ busyMessage
    else do  
        let parser = case (inputFormat,outputFormat) of
                ("tex","svg") -> texToSvg
                ("tex","html") -> texToHtml
                _ -> mdToHtml
        docData<- requireCheckJsonBody ::Handler EditorData
        preview<-liftIO $ parse Nothing userDir parser docData  
        return $ RepPlain $ toContent $ case preview of
            "\n"->""
            x->x

getDownloadR :: UserId -> EntryId -> Handler Html
getDownloadR _ entryId = do
    --cacheEntryPdf entryId
    pdfPath <- entryPdfPath entryId
    entry <- runDB $ get404 entryId
    userId <- requireAuthId
    if (entryStatus entry == Publish) || (entryUserId entry == userId) 
        then do
            sendFile "application/pdf" $ pdfPath
        else do
            notFound
    --return $ RepPlain $ toContent $ ("Download complete"::Text)

entryPdfPath :: EntryId -> Handler FilePath
entryPdfPath entryId = do
    entryCacheDir <- entryCacheDirectory entryId
    return $ entryCacheDir </> downloadPdfFileName

doesEntryPdfExist :: EntryId -> Handler Bool
doesEntryPdfExist entryId = do
    pdfPath <- entryPdfPath entryId
    liftIO $ doesFileExist pdfPath

purgeEntryPdf :: EntryId -> Handler ()
purgeEntryPdf entryId = do
    pdfPath <- entryPdfPath entryId
    liftIO $ do
        removeFile pdfPath `Import.catch` ((\_ -> return ())::IOException -> IO ())
        return ()

cacheEntryPdf :: EntryId -> Handler ()
cacheEntryPdf entryId = do
        urlRender <- getUrlRender
        --userId<-requireAuthId 
        (author, entry)<- runDB $ do 
            entry <- get404 entryId
            author <- get404 $ entryUserId entry
            return (author, entry)
        let docData = EditorData {
                editorContent = entryBody entry,
                editorPreamble = entryPreamble entry,
                editorCitation = entryCitation entry
            }
        let docData' = preProcessEditorData docData
        let maybeTextareaToText x = case x of
                    Just t -> unTextarea t
                    Nothing -> "coming soon..."-- empty file does not work well
        
        let (parser, content') = case (entryFormat entry) of
                Format "tex" -> (texToPdf, unlines ["\\noindent\\textbf{\\Huge " <> entryTitle entry <> "}\\\\" 
                                ,"written by " <> (userName author) <> " on " <> appName <> "\\\\"
                                ,"original link: " <> (urlRender (UserEntryR (entryUserId entry) entryId)) <> "\\\\"
                                ,"\\rule{\\linewidth}{1pt}"
                                ,""
                                , maybeTextareaToText $ editorContent docData'
                                ])
                    
                _ -> (mdToPdf, unlines ["# "<> entryTitle entry
                                , "written by " <> (userName author) <> " on " <> appName <> "  "
                                , "original link: " <> (urlRender (UserEntryR (entryUserId entry) entryId))
                                , ""
                                , "\\rule[5pt]{\\linewidth}{1pt}"
                                , ""
                                , ""
                                , maybeTextareaToText $ editorContent docData'
                                ])
        let editorData = docData' {editorContent = Just $ Textarea content'}
        tempDir<-userTemporaryDirectory --don't use this dirctory when using forkIO
        --tempDir <- liftIO $ getTemporaryDirectory >>= return . (</> "functor-network" </> "entry" </> (unpack $ toPathPiece entryId))
        pdfPath <- entryPdfPath entryId
        liftIO $ do
            removeFile pdfPath `Import.catch` ((\_ -> return ())::IOException -> IO ())
            parse (Just pdfPath) tempDir parser editorData >> return ()
            return ()

-- | cache entry
cacheEntry :: Entity Entry -> Handler ()
cacheEntry (Entity entryId entry) = do
    entryCacheDir <- entryCacheDirectory entryId
    tempDir <- userTemporaryDirectory
    let (titleParser, bodyParser) = case (entryFormat entry) of
            Format "tex" -> (texToHtmlSimple, texToHtml)
            _ -> (mdToHtmlSimple, mdToHtml)
    let docData = EditorData {
        editorContent = entryBody entry,
        editorPreamble = entryPreamble entry,
        editorCitation = entryCitation entry
    }
    --parse (Just (entryCacheDir </> "post.pdf")) tempDir pdfParser docData  

    {-let sourcePath = entryCacheDir </> "source"
    createDirectoryIfMissing True sourcePath
    let fileExtension = case (entryFormat entry) of
            Format "tex" -> "tex"
            _ -> "md"
    Data.Text.IO.writeFile (sourcePath </> "content" <.> fileExtension) $ maybeTextareaToText $ entryBody entry
    Data.Text.IO.writeFile (sourcePath </> "preamble.tex") $ maybeTextareaToText $ entryPreamble entry
    Data.Text.IO.writeFile (sourcePath </> "citation.bib") $ maybeTextareaToText $ entryCitation entry
    createArchive (entryCacheDir </> "download.zip") (packDirRecur Deflate mkEntrySelector entryCacheDir)
    removeDirectoryRecursive sourcePath-}
    liftIO $ do
        removeFile (entryCacheDir </> "title.html") `Import.catch` ((\_ -> return ())::IOException -> IO ())
        removeFile (entryCacheDir </> "body.html") `Import.catch` ((\_ -> return ())::IOException -> IO ())
        _<-parse (Just (entryCacheDir </> "title.html")) tempDir titleParser $ entryTitle entry
        _<-parse (Just (entryCacheDir </> "body.html")) tempDir bodyParser docData
        return ()
            
cacheDirectory :: Handler FilePath
cacheDirectory = do
    app <- getYesod
    let cacheDir = appCacheDir $ appSettings app
    return cacheDir

entryCacheDirectory :: EntryId -> Handler FilePath
entryCacheDirectory entryId = cacheDirectory >>= return . (</> (unpack $ toPathPiece entryId))

{-- | get App root directory
appWorkingDirectory :: IO FilePath
appWorkingDirectory = do
    exePath <- getExecutablePath  
    return $ if takeFileName exePath == "ghc" 
        then (takeDirectory $ takeDirectory $ takeDirectory $ takeDirectory $ takeDirectory $ takeDirectory $ takeDirectory exePath) </>  "functor-network"-- development
        else takeDirectory $ takeDirectory $ takeDirectory exePath -- production
--}

-- | Get user temporary directory
userTemporaryDirectory :: Handler FilePath
userTemporaryDirectory = do
    randomString<-liftIO $ fmap (take 10 . randomRs ('a','z')) newStdGen
    tempDir <- liftIO $ getTemporaryDirectory >>= \dir -> return $ dir </> "functor-network"
    
    maybeUserId<-maybeAuthId
    case maybeUserId of
        Nothing-> do
            maybeToken <-  fmap reqToken getRequest
            case maybeToken of
                Nothing-> notFound
                Just token-> do
                    return $ tempDir </> "anonymous" </> unpack (toPathPiece (token)) </> randomString
        Just userId-> return $ tempDir </> "user" </> unpack (toPathPiece userId) </> randomString

countActiveSubdirectories :: FilePath -> IO Int
countActiveSubdirectories dir = doesDirectoryExist dir >>= \exisitence ->
    if exisitence then do
        contents <- listDirectory dir
        subdirs <- filterM (\f -> doesDirectoryExist $ dir </> f) contents
        return $ length subdirs
    else return 0

{-isActiveDirectory :: FilePath -> IO Bool
isActiveDirectory dir = doesDirectoryExist dir >>= \exisitence ->
    if exisitence then do
        accessTime <- getAccessTime dir
        currentTime <- getCurrentTime
        let timeDiff = diffUTCTime currentTime accessTime
        if timeDiff < 60 * 60 
            then return True 
            else do --should not happen
                removeDirectoryRecursive dir 
                return False
    else return False-}

editorWidget :: Format -> Widget
editorWidget inputFormat = do
    maybeUserId <- maybeAuthId
    addScript $ StaticR editor_src_min_ace_js
    addScript $ StaticR editor_src_min_ext_language_tools_js
    --urlRender <- getUrlRender

    mcsrftoken <- fmap reqToken getRequest                                                                                                                  
    let csrftoken = case mcsrftoken of                                                                                                                      
                        Nothing -> "NO_TOKEN"                                                                                                               
                        Just t  -> t   
    let parserRoute =case inputFormat of
            Format "tex"->ParseR "tex" "html"
            _->ParseR "md" "html"
    $(widgetFile "editor")
    case inputFormat of
        Format "tex" -> do
            $(widgetFile "editor-tex")
        _ -> do
            $(widgetFile "editor-md")
