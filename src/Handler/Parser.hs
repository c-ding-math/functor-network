{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Parser (
    getUserTemporaryDirectory,
    parse,
    markItUpWidget,
    tagsWidget,
    postParserR
)where

--import qualified Prelude (writeFile)
--import System.Process
import System.Directory
--import Text.HTML.TagSoup 
import Import
import Parse.Parser(mdToHtml,texToHtml,EditorData(..))

type InputFormat=Text
type OutputFormat=Text

postParserR :: InputFormat->OutputFormat-> Handler RepPlain
postParserR inputFormat outputFormat = do
    let parser = case (inputFormat,outputFormat) of
            ("tex","html") -> texToHtml
            _ -> mdToHtml
    maybeUserId<-maybeAuthId
    docData<- requireCheckJsonBody ::Handler EditorData
    preview<-liftIO $ parse maybeUserId parser docData   
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
});

$(document).ready(function(){
    $('select').change(function(){
        var selected=$(this).children("option:selected").val();
        var format="md"
        if (selected=="2"){
            format="tex"
        }
        var currentUrl = window.location.href.split(/[?#]/)[0];
        window.location.href = currentUrl + '?format=' + format;
    });
});
        |]

-- | Widget for tags
tagsWidget::Path->[(Text,Text)]->Widget
tagsWidget piece tags=do
    toWidget [hamlet|
<ul>
    $forall (inputTag,outputTag) <-tags
        <li>
            <a href=@{TagR piece inputTag}> #{preEscapedToMarkup outputTag}
    |]

-- | parse
parse:: Maybe UserId -> (a->IO Text) -> a -> IO Text
parse maybeUserId parser docData = do
    currentWorkingDir<-getUserTemporaryDirectory maybeUserId
    createDirectoryIfMissing True currentWorkingDir  
    cleanUpDirectory currentWorkingDir
    --removeDirectoryRecursive currentWorkingDir
    withCurrentDirectory currentWorkingDir $ parser docData  

-- | Clean up a directory by removing all files and directories within it
cleanUpDirectory :: FilePath -> IO ()
cleanUpDirectory dir = do
  contents <- listDirectory dir
  let removeItem :: FilePath -> IO ()
      removeItem item = do
        let itemPath = dir </> item
        isDir <- doesDirectoryExist itemPath
        if isDir
          then do
            cleanUpDirectory itemPath
            removeDirectory itemPath
          else removeFile itemPath
  mapM_ removeItem contents

-- | get temporary directory for a user
getUserTemporaryDirectory:: Maybe UserId -> IO FilePath
getUserTemporaryDirectory maybeUserId=do
    tempDir <- getTemporaryDirectory
    return $ tempDir </> "math-blog" </> "user" </>  case maybeUserId of 
            Just userId->unpack (toPathPiece userId)
            _ -> "0" 
