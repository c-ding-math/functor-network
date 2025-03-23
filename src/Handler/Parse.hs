{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Parse (
    userTemporaryDirectory,
    parse,
    postParseR,
    editorWidget,
    countActiveSubdirectories
)where

import Import
--import Control.Concurrent
import System.Directory
import System.Environment (getExecutablePath)
import System.FilePath
import System.Random
import Parse.Parser(mdToHtml,texToHtml,EditorData(..))
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
                ("tex","html") -> texToHtml
                _ -> mdToHtml
        docData<- requireCheckJsonBody ::Handler EditorData
        preview<-liftIO $ parse userDir parser docData  
        return $ RepPlain $ toContent $ case preview of
            "\n"->""
            x->x

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

-- | parse
parse:: FilePath -> (a->IO Text) -> a -> IO Text
parse currentWorkingDir parser docData = do
    appDirectory<-appWorkingDirectory
    setCurrentDirectory appDirectory
    createDirectoryIfMissing True currentWorkingDir
    output<-withCurrentDirectory currentWorkingDir $ do
        output <- parser docData
        --Prelude.writeFile "output.html" $ unpack output
        return output
    --threadDelay 10000000
    removeDirectoryRecursive currentWorkingDir
    return $ output

-- | get App root directory
appWorkingDirectory :: IO FilePath
appWorkingDirectory = do
    exePath <- getExecutablePath  
    return $ if takeFileName exePath == "ghc" 
        then (takeDirectory $ takeDirectory $ takeDirectory $ takeDirectory $ takeDirectory $ takeDirectory $ takeDirectory exePath) </>  "functor-network"-- development
        else takeDirectory $ takeDirectory $ takeDirectory exePath -- production

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
