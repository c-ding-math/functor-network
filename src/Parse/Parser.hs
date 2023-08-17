{-# LANGUAGE QuasiQuotes           #-}
--{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE NoImplicitPrelude #-}

module Parse.Parser (
    mdToHtml,
    mdToHtmlSimple,
    texToHtml,
    texToHtmlSimple,
    scaleHeader,
    EditorData(..),
) where

--import qualified Prelude 
import Parse.Svg
import System.Process
--import System.Directory
--import Text.HTML.TagSoup 
import Text.Regex (mkRegexWithOpts, subRegex)
import Text.Regex.Posix
--import Import
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

mdToHtml::EditorData->IO Text
mdToHtml docData=do
    
    Prelude.writeFile ("yaml.yaml") $ textareaToYaml $ editorPreamble docData
    Prelude.writeFile ("bib.bib")  $ textareaToString $ editorCitation docData
    htmlString<-readProcess "pandoc" ["--sandbox", "-F", "pandoc-security", "--metadata-file=" ++ ("yaml.yaml"), "-F","pandoc-theorem", "-F","math-filter", "-C", "--bibliography=" ++ "bib.bib"] $ textareaToString $ editorContent docData
    return $ pack htmlString

mdToHtmlSimple::Text->IO Text
mdToHtmlSimple title=do
    titleString<-readProcess "pandoc" ["--sandbox", "-F", "pandoc-security", "-F","math-filter"] $ unpack $ title

    return $ pack $ removePTag titleString  

texToHtml::EditorData->IO Text
texToHtml docData=do
    
    Prelude.writeFile ("yaml.yaml") $ textareaToYaml $ editorPreamble docData
    Prelude.writeFile ("bib.bib")  $ textareaToString $ editorCitation docData
    htmlString<-readProcess "pandoc" ["--sandbox", "-F", "pandoc-security", "--metadata-file=" ++ ("yaml.yaml"), "-F","math-filter", "-C", "--bibliography=" ++ "bib.bib", "-f", "latex+raw_tex"] $ textareaToString $ editorContent docData
    return $ pack $ htmlString
            
texToHtmlSimple::Text->IO Text
texToHtmlSimple title=do
    titleString<-readProcess "pandoc" ["--sandbox", "-F", "pandoc-security", "-F","math-filter", "-f", "latex+raw_tex"] $ unpack $ title

    return $ pack $ removePTag titleString 

scaleHeader::Int->Text->Text
scaleHeader n title|n<=6= do
    let headerScale=[2.4,2.00,1.6,1.3,1.0,0.7]
    let widthMatches=getAllTextMatches $ unpack title =~ "width=\'([0-9]*\\.[0-9]*)pt\'" :: [String]
        widths=(Prelude.map stringToDouble widthMatches)
        heightMatches=getAllTextMatches $ unpack title =~ "height=\'([0-9]*\\.[0-9]*)pt\'" :: [String]
        heights=(Prelude.map stringToDouble heightMatches)
    let modify:: [Double]->[Double]->String->String
        modify (w:ws) (h:hs) str= do
            let newStr=subRegex (mkRegexWithOpts ("width=\'"++ show w ++"pt\'") False True) str ("width='" ++ (show (headerScale!!(n-1) * w)) ++ "pt'")
            let newStr2=subRegex (mkRegexWithOpts ("height=\'"++ show h ++"pt\'") False True) newStr ("height='" ++ (show (headerScale!!(n-1) * h)) ++ "pt'")
            modify ws hs newStr2
        modify _ _ str=str
    pack $ modify widths heights $ unpack title
        
scaleHeader _ title = title

textareaToString::Maybe Textarea->String
textareaToString  (Just t)=  (unpack .  unTextarea) t
textareaToString  _ = ""

textareaToYaml:: Maybe Textarea -> String
textareaToYaml (Just textarea)= unpack $ pack "preamble: |\n" <> (yamlBlock (unTextarea textarea)) where
    yamlBlock text=Data.Text.unlines $ (\x-> pack " " <> strip x) <$> Data.Text.lines text
textareaToYaml _ =""

removePTag::String->String
removePTag html =  subRegex (mkRegexWithOpts "<p[^>]*>(.*)</p>" False True) html ("\\1")
    
{-removePTag :: String -> String
removePTag html =  renderTags $ Prelude.filter (not . isBlockquote) $ parseTags html where
    isBlockquote (TagOpen "p" _) = True
    isBlockquote _ = False-}

{-removePTag :: String -> String
removePTag html =  case parseTags html of
    [TagOpen "p" _,tagString,TagClose "p"] -> fromTagText tagString
    _ -> html-}
