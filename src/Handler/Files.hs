{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell,
         TypeFamilies, MultiParamTypeClasses, FlexibleContexts, GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Files where

import System.FilePath
import System.Directory (removeFile, createDirectoryIfMissing)
import Import
import Yesod.Form.Bootstrap3
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS

uploadForm :: Html -> MForm Handler (FormResult (FileInfo), Widget)
uploadForm = renderBootstrap3 BootstrapBasicForm $ fileAFormReq (bfs MsgNewFile)

getFilesR :: Handler Html
getFilesR = do
    userId <- requireAuthId
    --urlRender <- getUrlRender
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
              <thead>
                <tr .header>
                    <th>
                        _{MsgFile}
                    <!--<th>
                        _{MsgDescription}-->
                    <th>
                        _{MsgLink}
                    <th>
                        _{MsgUploaded}
                    <th>
                        _{MsgActions}
              <tbody>
                $forall Entity fileId file <- files
                    <tr>
                        <td>
                            <a href=@{fileLink (Entity fileId file)}>
                                #{fileDescription file}
                        <td>
                            <input type=text readonly value=@{fileLink (Entity fileId file)}>         
                        <td>
                            #{formatDateStr $ fileInserted file}
                        <td .actions>
                            <a href=# .copy-link data-toggle=tooltip data-placement="top" title="click to copy">_{MsgCopy}
                            <a href=@{fileLink (Entity fileId file)} .view>_{MsgView}
                            <a .delete href=# data-delete-route=@{FileR fileId}>_{MsgDelete}
        
        <div>
            <form method=post enctype=#{uploadEnctype}>
                ^{uploadWidget}
                <input .btn.btn-default type=submit value=Upload>                    
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
    userFileDirectory <- getUserStaticDir $ Just userId
    ((result, _), _) <- runFormPost uploadForm
    msgRender <- getMessageRender
    isAjax <- isAjaxRequest

    case result of
        FormSuccess (fileInfo) -> do
            --urlRender <- getUrlRender
            let name = fileName fileInfo
                fileSource' = fileSource fileInfo
                
                allowedExtensions = [".jpg", ".jpeg", ".png", ".svg", ".ico", ".gif", ".tiff", ".webp", "jp2", ".txt", ".tex", ".md", ".bib", ".pdf", ".djvu", "docx", "pptx", "xlsx", "odt", "odp", "ods", "rtf", "html", "epub"] -- Allowed extensions
                fileExtension =toLower $ takeExtension $ unpack name
                
            if (not $ fileExtension `elem` allowedExtensions) then do
                if isAjax then
                    sendResponseStatus status415 $ msgRender $ MsgFileTypeNotAllowed
                else do
                    setMessageI MsgFileTypeNotAllowed
                    redirect FilesR     
            else do
                let maxSizeInMb = 16::Int
                    maxSize = maxSizeInMb * 1024 * 1024  -- Maximum file size in bytes (e.g., 10MB)
                size <- liftIO $ runConduitRes $ fileSource' .| CL.foldM (\acc bs -> pure (acc + fromIntegral (BS.length bs))) 0      
                if (size > maxSize) then do
                    if isAjax then
                        sendResponseStatus status413 $ msgRender $ MsgFileTooLarge maxSizeInMb
                        
                    else do 
                        setMessageI $ MsgFileTooLarge maxSizeInMb
                        redirect FilesR

                else do
                    currentTime <- liftIO getCurrentTime
                    let file = File {fileUserId =userId, fileFormat = Format (pack fileExtension), fileInserted = currentTime, fileDescription = name}
                    fileId  <- runDB $ insert file
                    liftIO $ do 
                        createDirectoryIfMissing True $ userFileDirectory
                        fileMove fileInfo $ userFileDirectory </> unpack (toPathPiece fileId) <.> fileExtension
                    if isAjax then do 
                        urlRender <- getUrlRender
                        sendResponseStatus status201 $ object ["url" .= urlRender (fileLink (Entity fileId file))]
                        
                    else do
                        setMessageI MsgFileSaved
                        redirect FilesR
        FormFailure _ -> 
            if isAjax then
                sendResponseStatus status400 $ msgRender $ MsgFormFailure
            else do
                setMessageI MsgFormFailure
                redirect FilesR
        _ -> 
            if isAjax then
                sendResponseStatus status400 $ msgRender $ MsgFormMissing
            else do
                setMessageI MsgFormMissing
                redirect FilesR
    
    

deleteFileR :: FileId -> Handler ()
deleteFileR fileId = do
    file <- runDB $ get404 fileId
    userFileDirectory <- getUserStaticDir $ Just $ fileUserId file
    let filePath = userFileDirectory </> case fileFormat file of
            Format extension -> unpack (toPathPiece fileId) <.> unpack extension

    liftIO $ removeFile filePath
    runDB $ delete fileId
    setMessageI MsgFileDeleted
    redirect FilesR

isAjaxRequest :: Handler Bool
isAjaxRequest = do
    maybeHeader <- lookupHeader "X-Requested-With"
    case maybeHeader of
        Just header -> return $ header == "XMLHttpRequest"
        Nothing -> return False

fileLink ::Entity File -> Route App
fileLink (Entity fileId (File userId (Format extension) _ _)) = StaticR $ StaticRoute ["files","user", toPathPiece userId,  pack $ unpack (toPathPiece fileId) <.> unpack extension] []

getUserStaticDir :: Maybe UserId -> Handler FilePath
getUserStaticDir mUserId = do
    staticDir <- appStaticDir . appSettings <$> getYesod
    case mUserId of
        Nothing -> do
            maybeToken <-  fmap reqToken getRequest
            case maybeToken of
                Nothing-> notFound
                Just token-> do
                    return $ staticDir </> "files" </> "anonymous" </> ( unpack  (toPathPiece token) )
        Just userId ->
            return $ staticDir </> "files" </> "user" </> ( unpack  (toPathPiece userId) )

--openConnectionCount :: Int
--openConnectionCount = 10

formatDateStr :: UTCTime -> String
formatDateStr t = formatTime defaultTimeLocale "%e %b %Y" t