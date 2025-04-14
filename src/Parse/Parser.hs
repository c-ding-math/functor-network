{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE NoImplicitPrelude #-}

module Parse.Parser (
    downloadPdfFileName,
    mdToPdf,
    texToPdf,
    mdToHtml,
    mdToHtmlSimple,
    texToHtml,
    texToHtmlSimple,
    scaleHeader,
    texToSvg,
    preProcessEditorData,
    EditorData(..),
) where

--import Control.Concurrent.Async
--import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Parse.KillOldProcesses(killOldProcesses)
import System.Process
import System.Exit
import System.FilePath.Posix
import Text.HTML.TagSoup 
import Text.HTML.Scalpel
import Text.RE.Replace
import Text.RE.TDFA
import Yesod.Form.Fields 
import Data.Text
import Data.Text.IO
import Data.Maybe
import System.Timeout
import GHC.Generics
import Data.Aeson
import System.Directory
import qualified Import 

data EditorData=EditorData{
        editorPreamble::Maybe Textarea
        ,editorContent::Maybe Textarea
        ,editorCitation::Maybe Textarea
    }  deriving (Generic, Show)

instance ToJSON EditorData where
instance FromJSON EditorData where

handleProcess :: Bool -> FilePath -> [String] -> String -> FilePath -> IO FilePath
handleProcess isInlineParser cmd args input output = do
    maybeResult <- timeout ((timeLimit+1)*1000000) $ readProcessWithExitCode cmd (args++[input]) ""
    case maybeResult of
        Just (exitCode, stdout, stderr) -> case exitCode of
            ExitSuccess -> when isInlineParser $ do
                mainText <- Data.Text.IO.readFile output
                Data.Text.IO.writeFile output $ case scrapeStringLike mainText (innerHTML $ "p") of
                        Just x -> x 
                        Nothing -> mainText
            _ -> do 
                let errorString = "Error! " ++ stdout ++ stderr ++"."
                renderError errorString
        Nothing -> do
            killOldProcesses timeLimit "latex"
            let errorString = "Error! " ++ "It takes too long to render the document. Please check whether there is an infinite loop in your LaTeX code."
            renderError errorString
    return output
    where 
        timeLimit = 10
        --renderError :: String -> IO ()
        renderError errorString = do
            mainText <- Import.catch (Data.Text.IO.readFile input) (\e -> return $ pack $ show (e :: Import.IOException))
            let tag = if isInlineParser then "span" else "div"
            
            let outputText = renderTags [
                    TagOpen "div" [("class", "alert-danger parser-message")],
                    TagText $ pack errorString,
                    TagClose "div",
                    TagOpen tag [("style", "width:520px")],
                    TagText $ mainText,
                    TagClose tag
                    ]
            Import.catch (Data.Text.IO.writeFile output $ outputText) ((\_ -> return ()):: Import.IOException -> IO ())

downloadPdfFileName :: FilePath
downloadPdfFileName = "download.pdf"

mdToPdf :: EditorData -> IO FilePath
mdToPdf docData = do
    let input = "md.md"
    Data.Text.IO.writeFile ("yaml.yaml") $ textareaToYaml $ editorPreamble docData
    Data.Text.IO.writeFile ("bib.bib")  $ unMaybeTextarea $ editorCitation docData
    Data.Text.IO.writeFile (input) $ unMaybeTextarea $ editorContent docData
    let output = downloadPdfFileName
    handleProcess False "pandoc" ["--sandbox", "-F", "pandoc-security", "--metadata-file", "yaml.yaml", "-F", "pandoc-theorem", "-C", "--bibliography=" ++ "bib.bib", "-o", output] input output
  
texToPdf :: EditorData -> IO FilePath
texToPdf docData'=do
    let output = "download.pdf"
    let input = takeBaseName output ++ ".tex"
    let docData = preProcessEditorData docData'
    let document = Data.Text.unlines [
            "\\documentclass{article}"
            , unMaybeTextarea (editorPreamble docData)
            , "\\begin{document}"
            , case editorContent docData of
                Just x -> unTextarea x
                _ -> "coming soon..."
            , if (isJust $ editorCitation docData) then "\\bibliography{bib.bib}\\bibliographystyle{plain}" else ""
            , "\\end{document}"
            ]
    Data.Text.IO.writeFile input document
    Data.Text.IO.writeFile "bib.bib" $ unMaybeTextarea $ editorCitation docData
    handleProcess False "latexmk" [] input output

mdToHtml :: EditorData -> IO FilePath
mdToHtml docData=do
    let input = "md.md"
    Data.Text.IO.writeFile ("yaml.yaml") $ textareaToYaml $ editorPreamble docData
    Data.Text.IO.writeFile ("bib.bib")  $ unMaybeTextarea $ editorCitation docData
    Data.Text.IO.writeFile (input) $ unMaybeTextarea $ editorContent docData
    let output = "output.html"
    handleProcess False "pandoc" ["--sandbox","-F", "pandoc-table", "-F", "pandoc-security", "--metadata-file", "yaml.yaml", "-F","pandoc-theorem", "-F", "math-filter", "-C", "--bibliography=" ++ "bib.bib" , "-o", output] input output

mdToHtmlSimple :: Text -> IO FilePath
mdToHtmlSimple title=do
    let input = "md.md"
    Data.Text.IO.writeFile (input) $ title
    let output = "simple.html"
    handleProcess True "pandoc" ["--sandbox", "-F", "pandoc-security", "-F", "math-filter", "-o", output] input output
    --return $ "<div style='width:520px'>"<>pack errorString<>"</div>"

texToHtml:: EditorData -> IO FilePath
texToHtml docData'=do
    let input = "tex.tex"
    let docData = preProcessEditorData docData'
    Data.Text.IO.writeFile ("yaml.yaml") $ textareaToYaml $ editorPreamble docData
    Data.Text.IO.writeFile ("bib.bib")  $ unMaybeTextarea $ editorCitation docData
    Data.Text.IO.writeFile (input) $ unMaybeTextarea $ editorContent docData
    let output = "output.html"
    handleProcess False "pandoc" ["--sandbox", "-F", "pandoc-table", "-F", "pandoc-security", "--metadata-file", "yaml.yaml", "-F", "math-filter", "-C", "--bibliography=" ++ "bib.bib", "-f", "latex+raw_tex", "-o", output] input output

texToHtmlSimple :: Text -> IO FilePath
texToHtmlSimple title=do
    let input = "tex.tex"
    let output = "output.html"
    Data.Text.IO.writeFile (input) $ title
    handleProcess True "pandoc" ["--sandbox", "-F", "pandoc-security", "-F", "math-filter", "-f", "latex+raw_tex", "-o", output] input output

texToSvg :: EditorData -> IO FilePath
texToSvg docData = do
    let input = "content.tex"
    let output = "output.svg"
    Data.Text.IO.writeFile ("preamble.txt") $ unMaybeTextarea $ editorPreamble docData
    Data.Text.IO.writeFile (input) $ unMaybeTextarea $ editorContent docData
    handleProcess False "tex-to-svg" ["preamble.txt", output] input output

-- should be replaced. This is a temporary solution
scaleHeader :: Int -> Text -> Text
scaleHeader n title|n<=6 =
    replaceAllCaptures SUB help $ title *=~ [re|([0-9]*\.[0-9]*)px|]
    where
        headerScale = [2.6,2.15,1.7,1.25,1.0,0.85] 
        help _ loc cap = case locationCapture loc of
            1-> Just $ pack $ show $ (headerScale!!(n-1)) * (read (unpack (capturedText cap)) :: Double)
            _ -> Nothing
scaleHeader _ title = title

preProcessEditorData::EditorData->EditorData
preProcessEditorData doc=do
    let content = unMaybeTextarea $ editorContent doc
    let preambleRegex = [reBlockSensitive|\\documentclass[^\{]*\{[^\}]*\}(.*)\\begin[[:blank:]]*\{document\}|]
        bodyRegex = [reBlockSensitive|\\begin[[:blank:]]*\{document\}(.*)\\end[[:blank:]]*\{document\}|]
    let preambleMatch = content ?=~ preambleRegex
        bodyMatch = content ?=~ bodyRegex
    let docWithNewPreamble = case captureTextMaybe [cp|1|] preambleMatch of
            Just x -> doc{editorPreamble=Just $ Textarea x}
            _ -> doc
        docWithNewContent = case captureTextMaybe [cp|1|] bodyMatch of
            Just x -> docWithNewPreamble{editorContent=Just $ Textarea x}
            _ -> docWithNewPreamble
    docWithNewContent

textareaToYaml:: Maybe Textarea -> Text
textareaToYaml (Just textarea)= "header-includes: |\n" <> (yamlBlock (unTextarea textarea)) where
    removeDocumentClass::Text->Text
    removeDocumentClass tex= tex *=~/ [edBlockSensitive|\\documentclass[^\{]*\{[^\}]*\}///|]
    yamlBlock tex= Data.Text.unlines $ (\x-> " " <> x) <$> ["```{=latex}"] ++ Data.Text.lines (removeDocumentClass tex) ++ ["```"]
textareaToYaml _ =""

unMaybeTextarea :: Maybe Textarea -> Text
unMaybeTextarea ta = case ta of
    Just (Textarea t) -> t
    Nothing -> ""