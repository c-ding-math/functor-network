{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell,
         TypeFamilies, MultiParamTypeClasses, FlexibleContexts, GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Files where

import System.FilePath
import System.Directory (removeFile, doesFileExist,createDirectoryIfMissing)
import Import
import Yesod.Form.Bootstrap3
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS

uploadForm :: Html -> MForm Handler (FormResult (FileInfo, Maybe Text, UTCTime), Widget)
uploadForm = renderBootstrap3 BootstrapBasicForm $ (,,)
    <$> fileAFormReq (bfs MsgNewFile)
    <*> pure Nothing
    <*> lift (liftIO getCurrentTime)

getFilesR :: Handler Html
getFilesR = do
    userId <- requireAuthId
    urlRender <- getUrlRender
    let fileLink::File->Text
        fileLink file=urlRender $ StaticR $ StaticRoute ["files","user",toPathPiece userId, pack (fileFilename file)] []
    ((_, uploadWidget), uploadEnctype) <- runFormPost uploadForm
    files <- runDB $ selectList [FileUserId==.userId] [Desc FileInserted]
    defaultLayout $ do
        --addStyleRemote "http://netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css"
        addWidget
        [whamlet|$newline never
        <h1>
            File Library
        $if not $ Import.null files
            <table .table>
                <tr>
                    <th>
                        _{MsgFile}
                    <!--<th>
                        _{MsgDescription}-->
                    <th>
                        _{MsgLink}
                    <th>
                        _{MsgUploaded}
                    <th>
                        _{MsgAction}
                $forall Entity fileId file <- files
                    <tr>
                        <td>
                            <a href=#{fileLink file}>
                                #{fileFilename file}
                        <!--<td>
                            $maybe description <- fileDescription file
                                #{description}-->
                        <td>
                            <input type=text readonly value=#{fileLink file}>         
                        <td>
                            #{formatDateStr $ fileInserted file}
                        <td .actions>
                            <a href=# .copy-link data-toggle=tooltip data-placement="top" title="click to copy">_{MsgCopy}
                            <a href=#{fileLink file} .view>_{MsgView}
                            <a .delete href=# data-delete-route=@{FileR fileId}>_{MsgDelete}
        
        <div>
            <form method=post enctype=#{uploadEnctype}>
                ^{uploadWidget}
                <input .btn type=submit value=Upload>                    
|]
  where  
    addWidget :: Widget
    addWidget = do

        -- message style
        --toWidget [lucius|.message { padding: 10px 0; background: #ffffed; } |]
        toWidget[lucius|
            table input[type=text]{width:100%;}
            table .actions a {padding-right:1em;}
            table .actions a:last-child {padding-right:0;}
        |]
        toWidget [julius|
    $(function(){
        function confirmDelete(link) {
            if (confirm("Are you sure that you want to delete the file?")) {
                deleteFile(link);
            };
        }
        function deleteFile(link) {
            $.ajax({
                type: "DELETE",
                url: link.attr("data-delete-route"),
            }).done(function(msg) {
                var table = link.closest("table");
                link.closest("tr").remove();
                var rowCount = $("td", table).length;
                if (rowCount === 0) {
                    table.remove();
                }
            });
        }
        $("a.delete").click(function() {
            confirmDelete($(this));
            return false;
        });
        $("a.copy-link").click(function(){
            var url=$(this).closest("tr").find("input[type='text']").val();
            navigator.clipboard.writeText(url);   
            var originalText = $(this).text(); 
            $(this).text('Copied'); 
            setTimeout(function() {
                $(this).text(originalText);
            }.bind(this), 1000);        
            return false;
        });

    });
    |]

postFilesR :: Handler Html
postFilesR = do
    --(userId, user) <- requireAuthPair
    userId <- requireAuthId
    userFileDirectory <- getUserStaticDir userId
    ((result, _), _) <- runFormPost uploadForm
    case result of
        FormSuccess (fileInfo, info, date) -> do
            --urlRender <- getUrlRender
            let name = fileName fileInfo
                fileSource' = fileSource fileInfo
                
                maxSizeInMb = 16::Int
                maxSize = maxSizeInMb * 1024 * 1024  -- Maximum file size in bytes (e.g., 10MB)
                allowedExtensions = [".jpg", ".jpeg", ".png", ".svg", ".ico", ".gif", ".tiff", ".webp", "jp2", ".txt", ".tex", ".md", ".bib", ".pdf", ".djvu", "docx", "pptx", "xlsx", "odt", "odp", "ods", "rtf", "html", "epub"] -- Allowed extensions
                fileExtension = takeExtension $ unpack name
            size <- liftIO $ runConduitRes $ fileSource' .| CL.foldM (\acc bs -> pure (acc + fromIntegral (BS.length bs))) 0
            if (size > maxSize) then do
                setMessageI $ MsgFileTooLarge maxSizeInMb
                redirect FilesR
            else 
                if (not $ fileExtension `elem` allowedExtensions) then do
                    setMessageI MsgFileTypeNotAllowed
                    redirect FilesR
                else do
                    eFile <- runDB $ insertBy (File {fileDirectory=userFileDirectory, fileFilename=unpack name, fileDescription= info, fileInserted= date, fileUserId=userId})
                    case eFile of 
                        Left _ -> setMessageI (MsgFileExists name)
                        Right _-> do
                            liftIO $ createDirectoryIfMissing True $ userFileDirectory
                            liftIO $ fileMove fileInfo $ userFileDirectory </> (unpack name)
                            setMessageI MsgFileSaved
                    redirect FilesR
        _ -> do
            setMessageI MsgSomethingWrong
            redirect FilesR

deleteFileR :: FileId -> Handler ()
deleteFileR fileId = do
    file <- runDB $ get404 fileId
    userFileDirectory <- getUserStaticDir (fileUserId file)
    let filePath = userFileDirectory </> (unpack (fileFilename file))
    liftIO $ removeFile filePath

    -- only delete from database if file has been removed from server
    stillExists <- liftIO $ doesFileExist filePath

    case (stillExists) of 
        True  -> do
            setMessageI MsgSomethingWrong
            redirect FilesR
        _ -> do
            runDB $ delete fileId
            setMessageI MsgFileDeleted
            redirect FilesR

--uploadDirectory ::  UserId -> FilePath
--uploadDirectory userId= "static"</>"files" </> "user" </> ( unpack  (toPathPiece userId) )
--uploadDirectory _="static"</>"files" </> "user" </> "0"

getUserStaticDir :: UserId -> Handler FilePath
getUserStaticDir userId = do
    staticDir <- appStaticDir . appSettings <$> getYesod
    return $ staticDir </> "files" </> "user" </> ( unpack  (toPathPiece userId) )

--openConnectionCount :: Int
--openConnectionCount = 10

formatDateStr :: UTCTime -> String
formatDateStr t = formatTime defaultTimeLocale "%e %b %Y" t