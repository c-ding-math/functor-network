{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Tool where

import Import
import Parse.Parser (EditorData(..))
import Handler.Parse (postParseR)
import Yesod.Form.Bootstrap3
--import Text.Shakespeare.Text (stext)


latexForm:: Maybe EditorData -> Form EditorData
latexForm mEntryInput= renderBootstrap3 BootstrapBasicForm $ EditorData
    <$> aopt textareaField preambleSettings (editorPreamble <$> mEntryInput)
    <*> aopt textareaField editorSettings (editorContent <$> mEntryInput)
    <*> aopt textareaField citationSettings (editorCitation <$> mEntryInput) where  
        editorSettings = FieldSettings
            { fsLabel = "\\begin{document}"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "content"
            , fsAttrs =[ ("class", "editor form-control"), ("placeholder", "Your main document goes here.")]}
        preambleSettings = FieldSettings
            { fsLabel = "\\documentclass{article}"
            , fsTooltip = Nothing
            , fsId = Just "preamble"
            , fsName = Just "preamble"
            , fsAttrs =[("class", "form-control collapse"), ("placeholder", "Include packages and define new commands here.")]}
        citationSettings = FieldSettings
            { fsLabel = "\\end{document}"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "citation"
            , fsAttrs =[("class", "hidden")]}

postToolR :: Text -> Handler RepPlain
postToolR "tex-to-svg" = postParseR "tex" "svg"
postToolR _ = notFound

getToolR :: Text -> Handler Html
getToolR "tex-to-svg" = do
    let parserRoute = ToolR "tex-to-svg"
    let defaultPreamble = "\\usepackage{amsmath, amssymb, amsfonts}\n\\newcommand{\\NN}{\\mathbb{N}}"
    let defaultContent = "Your main content goes here. Click the \\textbf{Download} button to get the SVG (Scalable Vector Graphics) file.\n\nHere is an example of display math: $$\\sum_{n=1}^{\\infty} \\frac{1}{n^2} = \\frac{\\pi^2}{6}.$$"

    (widget, enctype) <- generateFormPost $ latexForm $ Just $ EditorData (Just $ Textarea defaultPreamble) (Just $ defaultContent) (Just $ Textarea "")
    defaultLayout $ do
        setTitle "LaTeX to SVG"
        [whamlet|
            <h1>LaTeX to SVG
            <form method=post action=@{ToolR "tex-to-svg"} enctype=#{enctype}>
                ^{widget}
                <div .preview-download>
                    <button #preview-button .btn.btn-primary type=button value=preview>_{MsgPreview}
                    <button #download-button .btn.btn-primary type=button value=download>_{MsgDownload}
            <div .editor-preview>
                <div .entry-content-wrapper>
                    <div #preview>Click on the preview button to refresh the preview.
        |]
        toWidget [lucius|
                .caret.caret-up {
                    border-top-width: 0;
                    border-bottom: 4px solid;
                }
                .preview-download {
                    display: flex;
                    justify-content: space-between;
                    margin-bottom: 1em;
                }
                #preview{
                    width:520px;
                    margin-top: 1em;
                    margin-bottom: 1em;
                }
                .width100{
                    width:100%;
                }
        |]
        toWidget [julius|
            $(function(){
            
                var expandHint = $('<button type="button" class="btn btn-default width100 toggle" data-toggle="collapse" data-target="#preamble">Click here to edit your latex preamble.</button>');
                var expand = $('<a class="pull-right toggle" data-toggle="collapse" data-target="#preamble">expand <span class="caret"></span></a>');
                var collapse = $('<a class="pull-right hidden toggle" data-toggle="collapse" data-target="#preamble">collapse <span class="caret caret-up"></span></a>');
                expand.insertBefore($("textarea[name='preamble']"));
                collapse.insertBefore($("textarea[name='preamble']"));
                expandHint.insertBefore($("textarea[name='preamble']"));
                $(".toggle").click(function(){
                    $(".toggle").toggleClass("hidden");
                });
                
                $("form").submit(function(e){
                    e.preventDefault();
                    preview("");
                });
                $("#preview-button, #download-button").click(function(){
                    let action = $(this).val();
                    preview(action);
                });

            });
function preview(download) {
    var previewArea = $("#preview");
    var informationArea = $("#information");
    informationArea.insertAfter(previewArea);
    let entryData = JSON.stringify ({
                editorContent: $('textarea[name="content"]').val(),
                editorPreamble: $('textarea[name="preamble"]').val(),
                editorCitation: $('textarea[name="citation"]').val(),
            });
    $.ajax({
        type: "POST",
        dataType: "text",
        global: false,
        url: "@{parserRoute}",
        data: entryData,
        contentType:"application/json; charset=utf-8",
        //data: options.previewParserVar+'='+encodeURIComponent(parsedData),
        beforeSend: function() {
            $("#preview-button, #download-button").prop("disabled", true);
            previewArea.css("opacity", "0.5");
            informationArea.empty();
        },
        success: function(data) {
            previewArea.html(data);
            previewArea.find(".alert-danger.parser-message").each(function(){
                $(this).append("<span>Unable to correct the error? " + 
                    "<a href='@{FeedbackR}' target='_blank'>Report it</a>. " +
                    " We can help you!</span>").appendTo(informationArea);   
            });
            previewArea.find(".alert-warning.parser-message").each(function(){
                $(this).appendTo(informationArea);   
            });
        },
        complete: function() {
            previewArea.css("opacity", "1");
            if(download=="download"){
                if (previewArea.find("svg").length == 0) {
                    alert("Please correct your latex document before downloading the file.");
                } else {
                    downloadFile("download.svg", previewArea.html());
                }
            }
            $("#preview-button, #download-button").prop("disabled", false);
        },
    });
    return false;
}

function downloadFile(filename, text) {
  var element = document.createElement('a');
  element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
  element.setAttribute('download', filename);

  element.style.display = 'none';
  document.body.appendChild(element);

  element.click();

  document.body.removeChild(element);
}
        |]

getToolR _ = notFound


