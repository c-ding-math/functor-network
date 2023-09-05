{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Parser (
    userTemporaryDirectory,
    parse,
    markItUpWidget,
    tagsWidget,
    postParserR
)where

import Import
--import Control.Concurrent
import System.Directory
import System.Environment (getExecutablePath)
import System.FilePath
import System.Random
import Parse.Parser(mdToHtml,texToHtml,EditorData(..))
--import qualified Prelude
--import Data.Time.Clock

type InputFormat=Text
type OutputFormat=Text

postParserR :: InputFormat->OutputFormat-> Handler RepPlain
postParserR inputFormat outputFormat = do
    userDir<-userTemporaryDirectory
    subdirectoriesNumber<-liftIO $ countActiveSubdirectories $ takeDirectory userDir
    if subdirectoriesNumber > 0 then do
        let busyMessage::Text
            busyMessage= "busy..."
        return $ RepPlain $ toContent $ busyMessage
    else do  
        let parser = case (inputFormat,outputFormat) of
                ("tex","html") -> texToHtml
                _ -> mdToHtml
        docData<- requireCheckJsonBody ::Handler EditorData
        preview<-liftIO $ parse userDir parser docData  
        return $ RepPlain $ toContent $ preview

markItUpWidget ::Format->Format-> Widget
markItUpWidget inputFormat outputFormat=    do    --addScriptRemote "https://code.jquery.com/ui/1.13.2/jquery-ui.min.js"
        addScript $ StaticR scripts_jquery_ui_jquery_ui_min_js
        addStylesheet $ StaticR scripts_jquery_ui_jquery_ui_min_css
        addStylesheet $ StaticR scripts_jquery_ui_jquery_ui_additional_css
        addStylesheet $ StaticR scripts_jquery_ui_jquery_ui_structure_min_css
        --addStylesheet $ StaticR scripts_jquery_ui_dialog_css

        addStylesheet $ StaticR scripts_markItUp_markitup_skins_my_style_css
        addScript $ StaticR scripts_markItUp_markitup_jquery_markitup_js
        --addStylesheet $ StaticR scripts_markItUp_markitup_sets_markdown_style_css       
        if inputFormat==Format "tex" then do
            addScript $ StaticR scripts_markItUp_markitup_sets_tex_set_js
        else do
            addScript $ StaticR scripts_markItUp_markitup_sets_md_set_js
        
        let parserPath=case inputFormat of
                Format "tex"->ParserR "tex" "html"
                _->ParserR "md" "html"
        toWidget [julius|
$(document).ready(function(){
    var textarea=$("textarea.editor");
    var extraSettings={
        onCtrlEnter: {keepDefault:false,},
        previewInElement:'.markItUpFooter article', 
        previewParserPath: "@{parserPath}",
        previewAutoRefresh:false,     
        previewParser: function(content) {
            var data={
                    editorContent:content,
                    editorPreamble: $('textarea[name="preamble"]').val(),
                    editorCitation: $('textarea[name="citation"]').val(),
                };
                return (JSON.stringify(data));
            },                                    
    }
    textarea.markItUp(editorSettings,extraSettings);              

    //wrap textarea and preview area in a div
    var parent = textarea.closest('.markItUpContainer');
    var childrenToWrap = parent.children().not(':first-child');
    var wrapperDiv = $('<div>').attr('class', 'markItUpWrapper');
    childrenToWrap.wrapAll(wrapperDiv);       
    $('.markItUpFooter').append('<article></article>');

    //update preview on Ctrl + Enter
    textarea.attr("placeholder","Your content goes here. Press [Ctrl + Enter] to update preview");
    textarea.keyup(function(e){
		var updatePreviewButton = $(this).closest(".markItUp").find(".markItUpHeader .preview a[accesskey='0']").closest("li");
		if ((e.keyCode == 10 || e.keyCode == 13) && (e.ctrlKey || e.metaKey)) {
			updatePreviewButton.trigger("mouseup");	
		}
    })
    
});

$(document).ready(function(){
    $('select').change(function(){
        var selected=$(this).children("option:selected").val();
        var format="md"
        if (selected=="2"){
            format="tex"
        }
        var currentUrl = window.location.href.split(/[?#]/)[0]+ '?format=' + format;
        var elementId = window.location.href.match(/#[^?]*/);
        if (elementId){
            currentUrl=currentUrl+elementId;
        }
        
        window.location.href = currentUrl 

    });
});
        |]

-- | Widget for tags
tagsWidget::[(Text,Text)]->Widget
tagsWidget tags=do
    toWidget [hamlet|
<ul>
    $forall (inputTag,outputTag) <-tags
        <li>
            <a href=@{TagR inputTag}> #{preEscapedToMarkup outputTag}
    |]

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