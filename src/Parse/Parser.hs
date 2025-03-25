{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE NoImplicitPrelude #-}

module Parse.Parser (
    mdToPdf,
    texToPdf,
    mdToHtml,
    mdToHtmlSimple,
    texToHtml,
    texToHtmlSimple,
    scaleHeader,
    texToSvg,
    EditorData(..),
) where

--import Import
--import qualified Prelude 
--import Parse.Svg
import System.Process
import System.Exit
--import System.Directory
--import Text.HTML.TagSoup 
import Text.HTML.Scalpel (scrapeStringLike, attrs, innerHTML)
import Text.Regex.Posix
import Text.Regex (mkRegexWithOpts, subRegex, matchRegex)
import Yesod.Form.Fields 
import Data.Text
import GHC.Generics
import Data.Aeson

data EditorData=EditorData{
        editorPreamble::Maybe Textarea
        ,editorContent::Maybe Textarea
        ,editorCitation::Maybe Textarea
    }  deriving (Generic, Show)

instance ToJSON EditorData where
instance FromJSON EditorData where

mdToPdf::EditorData->IO Text
mdToPdf docData=do
    Prelude.writeFile ("yaml.yaml") $ removeDocumentClass $ textareaToYaml' $ editorPreamble docData
    Prelude.writeFile ("bib.bib")  $ textareaToString $ editorCitation docData
    (exitCode, pdfString, errorString)<-readProcessWithExitCode "pandoc" ["--sandbox", "-F", "pandoc-security", "--metadata-file", "yaml.yaml", "-F", "pandoc-theorem", "-C", "--bibliography=" ++ "bib.bib", "-o", "output.pdf"] $ textareaToString $ editorContent docData
    case exitCode of
        ExitSuccess -> do
            return $ "output.pdf"
        _ -> do
            return $ "<div style='width:520px'>"<>pack errorString<>"</div>"

texToPdf::EditorData->IO Text
texToPdf docData=do
    Prelude.writeFile ("yaml.yaml") $ removeDocumentClass $ textareaToYaml' $ editorPreamble docData
    Prelude.writeFile ("bib.bib")  $ textareaToString $ editorCitation docData
    (exitCode, pdfString, errorString)<-readProcessWithExitCode "pandoc" ["--sandbox", "-F", "pandoc-security", "--metadata-file", "yaml.yaml", "-F", "pandoc-theorem", "-C", "--bibliography=" ++ "bib.bib", "-o", "output.pdf"] $ textareaToString $ editorContent docData
    case exitCode of
        ExitSuccess -> do
            return $ "output.pdf"
        _ -> do
            return $ "<div style='width:520px'>"<>pack errorString<>"</div>"

mdToHtml::EditorData->IO Text
mdToHtml docData=do
    
    Prelude.writeFile ("yaml.yaml") $ removeDocumentClass $ textareaToYaml $ editorPreamble docData
    Prelude.writeFile ("bib.bib")  $ textareaToString $ editorCitation docData
    (exitCode, htmlString, errorString)<-readProcessWithExitCode "pandoc" ["--sandbox","-F", "pandoc-table", "-F", "pandoc-security", "--metadata-file", "yaml.yaml", "-F","pandoc-theorem", "-F", "math-filter", "-C", "--bibliography=" ++ "bib.bib"] $ textareaToString $ editorContent docData
    case exitCode of
        ExitSuccess -> do
            return $ pack $ htmlString
        _ -> do
            return $ "<div style='width:520px'>"<>pack errorString<>"</div>"
        
mdToHtmlSimple::Text->IO Text
mdToHtmlSimple title=do
    (exitCode, htmlString, errorString)<-readProcessWithExitCode "pandoc" ["--sandbox", "-F", "pandoc-security", "-F", "math-filter"] $ unpack $ title
    case exitCode of
        ExitSuccess -> do
            return $ pack $ removeOuterTag htmlString
        _ -> do
            return $ "<div style='width:520px'>"<>pack errorString<>"</div>"

texToHtml::EditorData->IO Text
texToHtml docData'=do
    let docData = preProcessEditorData docData'
    
    Prelude.writeFile ("yaml.yaml") $ removeDocumentClass $ textareaToYaml $ editorPreamble docData
    Prelude.writeFile ("bib.bib")  $ textareaToString $ editorCitation docData
    (exitCode, htmlString, errorString)<-readProcessWithExitCode "pandoc" ["--sandbox", "-F", "pandoc-table", "-F", "pandoc-security", "--metadata-file", "yaml.yaml", "-F", "math-filter", "-C", "--bibliography=" ++ "bib.bib", "-f", "latex+raw_tex"] $ textareaToString $ editorContent docData
    case exitCode of
        ExitSuccess -> do
            return $ pack $ htmlString
        _ -> do
            return $ "<div style='width:520px'>"<>pack errorString<>"</div>"
  where
    preProcessEditorData::EditorData->EditorData
    preProcessEditorData doc=do
        let preambleRegex = "\\\\documentclass[^\\{]*\\{[^\\}]*\\}(.*)\\\\begin[[:blank:]]*\\{document\\}"
        let bodyRegex = "\\\\begin[[:blank:]]*\\{document\\}(.*)\\\\end[[:blank:]]*\\{document\\}"
        let docWithNewPreamble = case matchRegex (mkRegexWithOpts preambleRegex False True) (textareaToString $ editorContent doc) of
                Just [x] -> doc{editorPreamble=Just $ Textarea $ pack x}
                _ -> doc
        let docWithNewContent = case matchRegex (mkRegexWithOpts bodyRegex False True) (textareaToString $ editorContent docWithNewPreamble) of
                Just [x] -> docWithNewPreamble{editorContent=Just $ Textarea $ pack x}
                _ -> docWithNewPreamble
        docWithNewContent
 
texToHtmlSimple::Text->IO Text
texToHtmlSimple title=do
    (exitCode, htmlString, errorString)<-readProcessWithExitCode "pandoc" ["--sandbox", "-F", "pandoc-security", "-F", "math-filter", "-f", "latex+raw_tex"] $ unpack $ title
    case exitCode of
        ExitSuccess -> do
            return $ pack $ removeOuterTag htmlString
        _ -> do
            return $ "<div style='width:520px'>"<>pack errorString<>"</div>"

texToSvg:: EditorData -> IO Text
texToSvg docData = do
    (exitCode, htmlString, errorString)<-readProcessWithExitCode "tex-to-svg" [textareaToString $ editorPreamble docData, textareaToString $ editorContent docData] ""
    case exitCode of
        ExitSuccess -> do
            return $ pack $ htmlString
        _ -> do
            return $ pack errorString

-- should be replaced. This is a temporary solution
scaleHeader::Int->Text->Text
scaleHeader n title|n<=6= do
    
    let headerScale= [2.6,2.15,1.7,1.25,1.0,0.85] 
    let temporaryReplacement="IDontBelieveThisStringWillEverOccurInUserInput"

    let widthMatches=case scrapeStringLike (unpack title) (attrs "width" $ "svg") of
            Just x -> x
            Nothing -> []
        widths=(Prelude.map stringToDouble widthMatches)
        heightMatches=case scrapeStringLike (unpack title) (attrs "height" $ "svg") of
            Just x -> x
            Nothing -> []
        heights=(Prelude.map stringToDouble heightMatches)
        depthMatches=case scrapeStringLike (unpack title) (attrs "style" $ "svg") of
            Just x -> x
            Nothing -> []
        depths=(Prelude.map stringToDouble depthMatches)
    let modify:: [Double]->[Double]->[Double]->String->String
        modify (w:ws) (h:hs) (d:ds) str= do
            let str1=subRegex (mkRegexWithOpts ("(width=\')"++ show w ++"(px\')") False True) str ("\\1" ++ temporaryReplacement ++ (show (headerScale!!(n-1) * w)) ++ "\\2")
                str1'=subRegex (mkRegexWithOpts ("(width=\")"++ show w ++"(px\")") False True) str1 ("\\1" ++ temporaryReplacement ++ (show (headerScale!!(n-1) * w)) ++ "\\2")
                str2=subRegex (mkRegexWithOpts ("(height=\')"++ show h ++"(px\')") False True) str1' ("\\1" ++ temporaryReplacement ++ (show (headerScale!!(n-1) * h)) ++ "\\2")
                str2'=subRegex (mkRegexWithOpts ("(height=\")"++ show h ++"(px\")") False True) str2 ("\\1" ++ temporaryReplacement ++ (show (headerScale!!(n-1) * h)) ++ "\\2")
                str3=subRegex (mkRegexWithOpts ("(vertical-align:-)"++ show d ++"(px;)") False True) str2' ("\\1" ++ temporaryReplacement ++ (show (headerScale!!(n-1) * d)) ++ "\\2")
                str3'=subRegex (mkRegexWithOpts ("(vertical-align:-)"++ show d ++"(px;)") False True) str3 ("\\1" ++ temporaryReplacement ++ (show (headerScale!!(n-1) * d)) ++ "\\2")
            modify ws hs ds str3'
        modify _ _ _ str=str
    pack $ subRegex (mkRegexWithOpts (temporaryReplacement) False True) (modify widths heights depths $ unpack title) ("")
        
scaleHeader _ title = title

textareaToString::Maybe Textarea->String
textareaToString  (Just t)=  (unpack .  unTextarea) t
textareaToString  _ = ""

textareaToYaml:: Maybe Textarea -> String
textareaToYaml (Just textarea)= unpack $ pack "preamble: |\n" <> (yamlBlock (unTextarea textarea)) where
    yamlBlock text=Data.Text.unlines $ (\x-> pack " " <> x) <$> [pack "```{=latex}"] ++ Data.Text.lines text ++ [pack "```"]
textareaToYaml _ =""

textareaToYaml':: Maybe Textarea -> String
textareaToYaml' (Just textarea)= unpack $ pack "header-includes: |\n" <> (yamlBlock (unTextarea textarea)) where
    yamlBlock text=Data.Text.unlines $ (\x-> pack " " <> x) <$> [pack "```{=latex}"] ++ Data.Text.lines text ++ [pack "```"]
textareaToYaml' _ =""

removeDocumentClass::String->String
removeDocumentClass tex= do
    let regexes=[
            "\\\\documentclass[^\\{]*\\{[^\\}]*\\}"
            --, "\\\\usepackage[^\\{]*\\{[[:space:]]*hyperref[[:space:]]*\\}"
            ]
    Prelude.foldl (\str regex-> subRegex (mkRegexWithOpts regex False True) str ("")) tex regexes
    --subRegex (mkRegexWithOpts "\\\\documentclass[^\\{]*\\{[^\\}]*\\}" False True) tex ("")

removeOuterTag::String->String
removeOuterTag html = case scrapeStringLike html (innerHTML $ "p") of
    Just x -> x
    Nothing -> html

stringToDouble:: String -> Double
stringToDouble s = do
    let doubleString = (s=~("[0-9]*\\.[0-9]*"::String) :: String)
        doubleString' =  case doubleString of
            '.' : _ -> "0" ++ doubleString
            _ -> doubleString
    read doubleString' :: Double