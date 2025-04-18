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
    parse
) where


import System.Process
import System.Exit
import Text.HTML.TagSoup 
import Parse.KillOldProcesses(killOldProcesses)
--import Control.Concurrent 
import Text.HTML.Scalpel
import System.FilePath.Posix
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


-- | parse
parse :: Maybe FilePath -> FilePath -> (FilePath -> a -> IO (CreateProcess, FilePath)) -> a -> IO Text
parse mFileName tmpDir parser docData = do

    createDirectoryIfMissing True tmpDir

    (process, output) <- parser tmpDir docData
    maybeResult <- timeout ((timeLimit+1)*1000000) $ Import.catch (readCreateProcessWithExitCode (process {cwd = Just tmpDir}) "") (\e -> return (ExitFailure 1, "", show (e :: Import.IOException))) -- readCreateProcessWithExitCode can still throw an exception when using the pdf parser: hGetContents: invalid argument (invalid byte sequence).
    renderedText <- case maybeResult of
        Just (exitCode, stdout, stderr) -> case exitCode of
            ExitSuccess -> 
                if takeExtension output == ".pdf"
                    then return $ pack output
                    else do 
                        outputText <- Import.catch (Data.Text.IO.readFile $ tmpDir</>output) (\e -> return $ pack $ show (e :: Import.IOException))
                        let isInlineParser = takeBaseName output == "simple"
                        return $ if isInlineParser
                            then case scrapeStringLike outputText (innerHTML $ "p") of
                                Just x -> x 
                                Nothing -> outputText
                            else outputText
                        
            _ -> do 
                let errorString = "Error! " ++ stdout ++ stderr ++"."
                renderError errorString
        Nothing -> do
            killOldProcesses timeLimit "latex"
            let errorString = "Error! " ++ "It takes too long to render the document. Please check whether there is an infinite loop in your LaTeX code."
            renderError errorString
    case mFileName of
        Nothing -> return ()
        Just cache -> do
            createDirectoryIfMissing True (takeDirectory cache)
            Import.catch (copyFile (tmpDir</>output) cache) ((const (return ())) :: Import.IOException -> IO ())
            return ()
    removeDirectoryRecursive tmpDir
    return $ renderedText
  where 
        timeLimit = 10
        renderError errorString = return $ 
            renderTags [
                    TagOpen "div" [("class", "alert-danger parser-message")],
                    TagText $ pack errorString,
                    TagClose "div"
                    ]

downloadPdfFileName :: FilePath
downloadPdfFileName = "download.pdf"

mdToPdf :: FilePath -> EditorData -> IO (CreateProcess, FilePath)
mdToPdf direcotry docData = do
    let input = "md.md"
    Data.Text.IO.writeFile (direcotry</>"yaml.yaml") $ textareaToYaml $ editorPreamble docData
    Data.Text.IO.writeFile (direcotry</>"bib.bib")  $ unMaybeTextarea $ editorCitation docData
    Data.Text.IO.writeFile (direcotry</>input) $ unMaybeTextarea $ editorContent docData
    let output = downloadPdfFileName
    return (proc "pandoc" ["--sandbox", "-F", "pandoc-security", "--metadata-file", "yaml.yaml", "-F", "pandoc-theorem", "-C", "--bibliography=" ++ "bib.bib", "-o", output, input], output)
    --handleProcess False "pandoc" ["--sandbox", "-F", "pandoc-security", "--metadata-file", "yaml.yaml", "-F", "pandoc-theorem", "-C", "--bibliography=" ++ "bib.bib", "-o", output] input output
  
texToPdf :: FilePath -> EditorData -> IO (CreateProcess, FilePath)
texToPdf direcotry docData'=do
    let output = downloadPdfFileName
    let input = takeBaseName output ++ ".tex"
    let docData = preProcessEditorData docData'
    let document = Data.Text.unlines [
            "\\documentclass{article}"
            , "\\usepackage{biblatex}"
            , "\\addbibresource{bib.bib}"
            , unMaybeTextarea (editorPreamble docData)
            , "\\begin{document}"
            , case editorContent docData of
                Just x -> unTextarea x
                _ -> "coming soon..."
            , if (isJust $ editorCitation docData) then "\\printbibliography[heading=none]" else ""--if not cited?
            , "\\end{document}"
            ]
    Data.Text.IO.writeFile (direcotry</>input) document
    Data.Text.IO.writeFile (direcotry</>"bib.bib") $ unMaybeTextarea $ editorCitation docData
    return (proc "latexmk" ["-pdf", "-halt-on-error", "-interaction=nonstopmode", input], output)
    --handleProcess False "latexmk" [] input output

mdToHtml :: FilePath -> EditorData -> IO (CreateProcess, FilePath)
mdToHtml direcotry docData=do
    let input = "md.md"
    Data.Text.IO.writeFile (direcotry</>"yaml.yaml") $ textareaToYaml $ editorPreamble docData
    Data.Text.IO.writeFile (direcotry</>"bib.bib")  $ unMaybeTextarea $ editorCitation docData
    Data.Text.IO.writeFile (direcotry</>input) $ unMaybeTextarea $ editorContent docData
    let output = "output.html"
    return (proc "pandoc" ["--sandbox", "-F", "pandoc-table", "-F", "pandoc-security", "--metadata-file", "yaml.yaml", "-F","pandoc-theorem", "-F", "math-filter", "-C", "--bibliography=" ++ "bib.bib" , "-o", output, input], output)
    --handleProcess False "pandoc" ["--sandbox","-F", "pandoc-table", "-F", "pandoc-security", "--metadata-file", "yaml.yaml", "-F","pandoc-theorem", "-F", "math-filter", "-C", "--bibliography=" ++ "bib.bib" , "-o", output] input output

mdToHtmlSimple :: FilePath -> Text -> IO (CreateProcess, FilePath)
mdToHtmlSimple direcotry title=do
    let input = "md.md"
    Data.Text.IO.writeFile (direcotry</>input) $ title
    let output = "simple.html" -- temporary solution.
    return (proc "pandoc" ["--sandbox", "-F", "pandoc-security", "-F", "math-filter", "-o", output, input], output)
    --handleProcess True "pandoc" ["--sandbox", "-F", "pandoc-security", "-F", "math-filter", "-o", output] input output
    --return $ "<div style='width:520px'>"<>pack errorString<>"</div>"

texToHtml:: FilePath -> EditorData -> IO (CreateProcess, FilePath)
texToHtml direcotry docData'=do
    let input = "tex.tex"
    let docData = preProcessEditorData docData'
    Data.Text.IO.writeFile (direcotry</>"yaml.yaml") $ textareaToYaml $ editorPreamble docData
    Data.Text.IO.writeFile (direcotry</>"bib.bib")  $ unMaybeTextarea $ editorCitation docData
    Data.Text.IO.writeFile (direcotry</>input) $ unMaybeTextarea $ editorContent docData
    let output = "output.html"
    return (proc "pandoc" ["--sandbox", "-F", "pandoc-table", "-F", "pandoc-security", "--metadata-file", "yaml.yaml", "-F", "math-filter", "-C", "--bibliography=" ++ "bib.bib", "-f", "latex+raw_tex", "-o", output, input], output)
    --handleProcess False "pandoc" ["--sandbox", "-F", "pandoc-table", "-F", "pandoc-security", "--metadata-file", "yaml.yaml", "-F", "math-filter", "-C", "--bibliography=" ++ "bib.bib", "-f", "latex+raw_tex", "-o", output] input output

texToHtmlSimple :: FilePath -> Text -> IO (CreateProcess, FilePath)
texToHtmlSimple direcotry title=do
    let input = "tex.tex"
    let output = "simple.html" -- temporary solution
    Data.Text.IO.writeFile (direcotry</>input) $ title
    return (proc "pandoc" ["--sandbox", "-F", "pandoc-security", "-F", "math-filter", "-f", "latex+raw_tex", "-o", output, input], output)
    --handleProcess True "pandoc" ["--sandbox", "-F", "pandoc-security", "-F", "math-filter", "-f", "latex+raw_tex", "-o", output] input output

texToSvg :: FilePath -> EditorData -> IO (CreateProcess, FilePath)
texToSvg direcotry docData = do
    let input = "content.tex"
    let output = "output.svg"
    Data.Text.IO.writeFile (direcotry</>"preamble.txt") $ unMaybeTextarea $ editorPreamble docData
    Data.Text.IO.writeFile (direcotry</>input) $ unMaybeTextarea $ editorContent docData
    return (proc "tex-to-svg" ["preamble.txt", output, input], output)
    --handleProcess False "tex-to-svg" ["preamble.txt", output] input output

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