{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Slug where

import Import
import System.Directory
import Yesod.Form.Bootstrap3
import Codec.Archive.Zip
--import qualified Data.ByteString.Lazy as BL


theForm :: Form Text
theForm = renderBootstrap3 BootstrapBasicForm $ areq textField "Slug" Nothing

getSlugR :: Handler Html
getSlugR = do
    (widget, enctype) <- generateFormPost $ theForm
    defaultLayout $ do
        setTitle "Slug"
        [whamlet|
            <h1>Slug
            <form method=post action=@{SlugR} enctype=#{enctype}>
                ^{widget}
                <button>Submit
        |]

postSlugR :: Handler Html
postSlugR = do 
    ((result, widget), enctype) <- runFormPost $ theForm
    let content = case result of
            FormSuccess slug -> slug
            _ -> "nothing"
    let dir = "../delete-me"
    liftIO $ do
        
        createDirectoryIfMissing True dir
        Prelude.writeFile (dir Prelude.++ "/d.txt") $ unpack content
    
        -- Create an empty archive /home/ding/functor-network/stack.yaml
        --archive <- addFilesToArchive [OptRecursive] emptyArchive ["/home/ding/delete-me/d.txt"]

        -- Write the archive to a file
        --liftIO $ BL.writeFile (dir Prelude.++"/archive.zip") (fromArchive archive)

        createArchive (dir Prelude.++"/archive.zip") (packDirRecur Deflate mkEntrySelector dir)
        
    sendFile "application/zip" (dir Prelude.++"/archive.zip")

    --sendFile "text/plain" (dir Prelude.++ "/d.txt")

