{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Parse where

import Import
import Control.Concurrent
import System.Directory
--import System.Environment (getExecutablePath)
import System.FilePath
import System.Random
import Parse.Parser(texToPdf, texToHtml, texToHtmlSimple, mdToPdf, mdToHtml, mdToHtmlSimple, texToSvg, EditorData(..))
import Data.Text.IO
import qualified Data.Text as T
import Codec.Archive.Zip
import qualified Prelude
--import Control.Exception

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

getDownloadR :: EntryId -> Handler TypedContent
getDownloadR entryId = do
    entry<-runDB $ get404 entryId
    entryCacheDir <- cacheDirectory >>= return . (</> (unpack $ toPathPiece entryId))
    liftIO $ cacheEntry entryCacheDir (Entity entryId entry)
    sendFile "application/zip" $ entryCacheDir </> "download.zip"

-- | Get user temporary directory
userTemporaryDirectory :: Handler FilePath
userTemporaryDirectory = do
    randomString<-liftIO $ fmap (take 10 . randomRs ('a','z')) newStdGen
    {-appDirectory<-liftIO appWorkingDirectory
    tempDir <- if takeFileName appDirectory == "functor-network" 
        then liftIO $ getTemporaryDirectory >>= \dir -> return $ dir </> "functor-network"-- development
        else return $ appDirectory </> "working"-- production-}
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

-- | cache entry

cacheEntry :: FilePath -> Entity Entry -> IO ()
cacheEntry entryCacheDir (Entity entryId entry) = do
            let tempDir = entryCacheDir </> "temp"
            createDirectoryIfMissing True tempDir
            let (pdfParser,titleParser, bodyParser) = case (entryFormat entry) of
                    Format "tex" -> (texToPdf, texToHtmlSimple, texToHtml)
                    _ -> (mdToPdf, mdToHtmlSimple, mdToHtml)
            let docData = EditorData {
                editorContent = entryBody entry,
                editorPreamble = entryPreamble entry,
                editorCitation = entryCitation entry
            }
            
            parse (Just (entryCacheDir </> "title.html")) tempDir titleParser $ entryTitle entry
            parse (Just (entryCacheDir </> "body.html")) tempDir bodyParser docData
            parse (Just (entryCacheDir </> "post.pdf")) tempDir pdfParser docData  

            let sourcePath = entryCacheDir </> "source"
            createDirectoryIfMissing True sourcePath
            let maybeTextareaToText x = case x of
                    Just t -> unTextarea t
                    Nothing -> " "-- empty file does not work well
            let fileExtension = case (entryFormat entry) of
                    Format "tex" -> "tex"
                    _ -> "md"
            Data.Text.IO.writeFile (sourcePath </> "content" <.> fileExtension) $ maybeTextareaToText $ entryBody entry
            Data.Text.IO.writeFile (sourcePath </> "preamble.tex") $ maybeTextareaToText $ entryPreamble entry
            Data.Text.IO.writeFile (sourcePath </> "citation.bib") $ maybeTextareaToText $ entryCitation entry
            createArchive (entryCacheDir </> "download.zip") (packDirRecur Deflate mkEntrySelector entryCacheDir)
            --removeDirectoryRecursive sourcePath
            

-- | parse
parse:: Maybe FilePath -> FilePath -> ( a -> IO Text) -> a -> IO Text
parse mFileName tmpDir parser docData = do
    --appDirectory<-appWorkingDirectory
    --setCurrentDirectory appDirectory
    appDirectory <- getCurrentDirectory
    createDirectoryIfMissing True tmpDir
    output <- withCurrentDirectory tmpDir $ parser docData
    case mFileName of
        Nothing -> return ()
        Just cache -> do
            createDirectoryIfMissing True (takeDirectory cache)
            if parser == texToPdf || parser == mdToPdf 
                then handle handler $ copyFile (tmpDir </> (unpack output)) cache
                else handle handler $ Data.Text.IO.writeFile cache $ output       
    --threadDelay 10000000
    removeDirectoryRecursive tmpDir
    return $ output
    where
        handler::SomeException->IO ()
        handler _ = return ()

cacheDirectory :: Handler FilePath
cacheDirectory = do
    app <- getYesod
    let cacheDir = appCacheDir $ appSettings app
    liftIO $ createDirectoryIfMissing True cacheDir
    return cacheDir

{-- | get App root directory
appWorkingDirectory :: IO FilePath
appWorkingDirectory = do
    exePath <- getExecutablePath  
    return $ if takeFileName exePath == "ghc" 
        then (takeDirectory $ takeDirectory $ takeDirectory $ takeDirectory $ takeDirectory $ takeDirectory $ takeDirectory exePath) </>  "functor-network"-- development
        else takeDirectory $ takeDirectory $ takeDirectory exePath -- production
--}

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
