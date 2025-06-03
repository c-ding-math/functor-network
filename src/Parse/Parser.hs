{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE NoImplicitPrelude #-}

module Parse.Parser (
    unMaybeTextarea,
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
import Text.Shakespeare.Text


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
    maybeResult <- timeout ((timeLimit+1)*1000000) $ Import.catch (readCreateProcessWithExitCode (process {cwd = Just tmpDir, create_group = True}) "") (\e -> return (ExitFailure 1, "", show (e :: Import.IOException))) -- readCreateProcessWithExitCode can still throw an exception when using the pdf parser: hGetContents: invalid argument (invalid byte sequence).
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
        timeLimit = 60
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
    Data.Text.IO.writeFile (direcotry</>"lean.xml") leanXml
    let output = "output.html"
    return (proc "pandoc" ["--sandbox", "-F", "pandoc-table", "-F", "pandoc-security", "--metadata-file", "yaml.yaml", "-F","pandoc-theorem", "-F", "math-filter", "-C", "--bibliography=" ++ "bib.bib" , "--syntax-definition=lean.xml", "-o", output, input], output)
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
    Data.Text.IO.writeFile (direcotry</>"lean.xml") leanXml
    let output = "output.html"
    return (proc "pandoc" ["--sandbox", "-F", "pandoc-table", "-F", "pandoc-security", "--metadata-file", "yaml.yaml", "-F", "math-filter", "-C", "--bibliography=" ++ "bib.bib", "-f", "latex+raw_tex", "--syntax-definition=lean.xml", "-o", output, input], output)
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
    removeMinted::Text->Text
    removeMinted tex = (tex *=~/ [edBlockSensitive|\\usepackage[^\{]*\{[[:blank:]]*minted[[:blank:]]*\}///|]) *=~/ [edBlockSensitive|\\usemintedstyle[^\{]*\{[^\}]*\}///|]
    yamlBlock tex= Data.Text.unlines $ (\x-> " " <> x) <$> ["```{=latex}"] ++ Data.Text.lines (removeMinted $ removeDocumentClass tex) ++ ["```"]
textareaToYaml _ =""

unMaybeTextarea :: Maybe Textarea -> Text
unMaybeTextarea ta = case ta of
    Just (Textarea t) -> t
    Nothing -> ""

leanXml = Import.toStrict [stext|
<?xml version="1.0" encoding="UTF-8"?>
<!--
  based on pygments highlighting file for Lean4 (https://github.com/leanprover/lean4/blob/master/doc/latex/lean4.py)
  and VSCode highlighting file for Lean4 (https://github.com/leanprover/vscode-lean4/blob/master/vscode-lean4/syntaxes/lean4.json).
-->
<language name="Lean" version="1" kateversion="2.4" section="Sources" extensions="*.lean;*.lean4"
    mimetype="text/x-lean" license="MIT" author="Hagb (Junyu Guo) (hagb@hagb.name)">

    <highlighting>

        <!-- {
        "name": "Lean 4",
        "scopeName": "source.lean4",
        "fileTypes": [],
        "patterns": [-->

        <!-- TODO { "include": "#comments" }, -->
        <!-- { "match": "\\b(Prop|Type|Sort)\\b", "name": "storage.type.lean4" }, -->
        <list name="Types"> <!-- dsBuiltIn -->
            <item>Prop</item>
            <item>Type</item>
            <item>Sort</item>
        </list>
        <!-- { "match": "\\battribute\\b\\s*\\[[^\\]]*\\]", "name": "storage.modifier.lean4" }, -->
        <list name="attribute"> <!-- dsAttribute -->
            <item>attribute</item>
        </list>

        <!-- { "match": "@\\[[^\\]]*\\]", "name": "storage.modifier.lean4" },-->
        <!--{
            "match":
        "\\b(?<!\\.)(global|local|scoped|partial|unsafe|private|protected|noncomputable)(?!\\.)\\b",
            "name": "storage.modifier.lean4"
          }, -->
        <list name="Modifiers"> <!-- dsAttribute -->
            <item>global</item>
            <item>local</item>
            <item>scoped</item>
            <item>partial</item>
            <item>unsafe</item>
            <item>private</item>
            <item>protected</item>
            <item>noncomputable</item>
        </list>
        <!-- { "match": "\\bsorry\\b", "name": "invalid.illegal.lean4" }, -->
        <list name="sorry"> <!-- dsWarning -->
            <item>sorry</item>
            <item>admit</item>
        </list>
        <!-- { "match": "#(print|eval|reduce|check|check_failure)\\b", "name": "keyword.other.lean4"
        }, -->
        <list name="Commands"> <!-- dsPreprocessor -->
            <item>#print</item>
            <item>#eval</item>
            <item>#reduce</item>
            <item>#check</item>
            <item>#check_failure</item>
            <item>#align</item>
        </list>
        <!-- TODO {
            "match": "\\bderiving\\s+instance\\b",
            "name": "keyword.other.command.lean4"
          }, -->
        <!-- {
            "begin":
        "\\b(?<!\\.)(inductive|coinductive|structure|theorem|axiom|abbrev|lemma|def|instance|class|constant)\\b\\s+(\\{[^}]*\\})?",
            "beginCaptures": {
              "1": {"name": "keyword.other.definitioncommand.lean4"}
            },
            "patterns": [
              {"include": "#comments"},
              {"include": "#definitionName"},
              {"match": ","}
            ],
            "end": "(?=\\bwith\\b|\\bextends\\b|\\bwhere\\b|[:\\|\\(\\[\\{⦃<>])",
            "name": "meta.definitioncommand.lean4"
          }, -->
        <list name="Definitions"> <!-- dsKeyword --> <!-- dsFunction until with -->
            <item>inductive</item>
            <item>coinductive</item>
            <item>structure</item>
            <item>theorem</item>
            <item>axiom</item>
            <item>abbrev</item>
            <item>lemma</item>
            <item>def</item>
            <item>instance</item>
            <item>class</item>
            <item>constant</item>
        </list>
        <list name="DefinitionsAfterName">
            <item>with</item>
            <item>extends</item>
            <item>where</item>
            <!-- <item>:</item>
            <item>|</item>
            <item>(</item>
            <item>[</item>
            <item>{</item>
            <item>⦃</item>
            <item><</item>
            <item>></item> -->
        </list>
        <!-- {
            "match":
        "\\b(?<!\\.)(theorem|show|have|from|suffices|nomatch|def|class|structure|instance|set_option|initialize|builtin_initialize|example|inductive|coinductive|axiom|constant|universe|universes|variable|variables|import|open|export|theory|prelude|renaming|hiding|exposing|do|by|let|extends|mutual|mut|where|rec|syntax|macro_rules|macro|deriving|fun|section|namespace|end|infix|infixl|infixr|postfix|prefix|notation|abbrev|if|then|else|calc|match|with|for|in|unless|try|catch|finally|return|continue|break)(?!\\.)\\b",
            "name": "keyword.other.lean4"
          }, -->
        <list name="import"> <!-- dsImport -->
            <item>import</item>
        </list>
        <list name="ControlFlows"> <!-- dsControlFlow -->
            <item>if</item>
            <item>then</item>
            <item>else</item>
            <item>calc</item>
            <item>match</item>
            <item>try</item>
            <item>catch</item>
            <item>finally</item>
            <item>return</item>
            <item>continue</item>
            <item>break</item>
            <item>for</item>
            <item>do</item>
        </list>
        <list name="Keywords"> <!-- dsKeyword -->
            <item>theorem</item>
            <item>def</item>
            <item>class</item>
            <item>axiom</item>
            <item>constant</item>
            <item>universe</item>
            <item>universes</item>
            <item>variable</item>
            <item>variables</item>
            <item>example</item>
            <item>structure</item>
            <item>instance</item>
            <item>postfix</item>
            <item>prefix</item>
            <item>notation</item>
            <item>abbrev</item>
            <item>infix</item>
            <item>infixl</item>
            <item>infixr</item>
            <item>show</item>
            <item>have</item>
            <item>from</item>
            <item>suffices</item>
            <item>nomatch</item>
            <item>set_option</item>
            <item>initialize</item>
            <item>builtin_initialize</item>
            <item>inductive</item>
            <item>coinductive</item>
            <item>open</item>
            <item>export</item>
            <item>theory</item>
            <item>prelude</item>
            <item>renaming</item>
            <item>hiding</item>
            <item>exposing</item>
            <item>by</item>
            <item>let</item>
            <item>extends</item>
            <item>mutual</item>
            <item>mut</item>
            <item>where</item>
            <item>rec</item>
            <item>syntax</item>
            <item>macro_rules</item>
            <item>macro</item>
            <item>deriving</item>
            <item>fun</item>
            <item>section</item>
            <item>namespace</item>
            <item>end</item>
            <item>with</item>
            <item>in</item>
            <item>unless</item>
            <item>at</item>
            <item>use</item>
            <item>begin</item>
            <item>termination_by</item>
            <item>decreasing_by</item>
        </list>
        <list name="Tactics">
            <item>intro</item>
            <item>intros</item>
            <item>rename</item>
            <item>revert</item>
            <item>clear</item>
            <item>subst</item>
            <item>subst_vars</item>
            <item>assumption</item>
            <item>contradiction</item>
            <item>apply</item>
            <item>exact</item>
            <item>refine</item>
            <item>refine'</item>
            <item>constructor</item>
            <item>case</item>
            <item>case'</item>
            <item>all_goals</item>
            <item>any_goals</item>
            <item>trace</item>
            <item>first</item>
            <item>rotate_left</item>
            <item>rotate_right</item>
            <item>try</item>
            <item>rfl</item>
            <item>rfl'</item>
            <item>ac_rfl</item>
            <item>sorry</item>
            <item>admit</item>
            <item>infer_instance</item>
            <item>change</item>
            <item>rewrite</item>
            <item>rw</item>
            <item>injection</item>
            <item>injections</item>
            <item>simp</item>
            <item>simp_all</item>
            <item>dsimp</item>
            <item>delta</item>
            <item>unfold</item>
            <item>refine_lift</item>
            <item>have</item>
            <item>suffices</item>
            <item>let</item>
            <item>show</item>
            <item>refine_lift'</item>
            <item>have'</item>
            <item>let'</item>
            <item>induction</item>
            <item>generalize</item>
            <item>cases</item>
            <item>rename_i</item>
            <item>repeat</item>
            <item>trivial</item>
            <item>split</item>
            <item>stop</item>
            <item>specialize</item>
            <item>unhygienic</item>
            <item>fail</item>
            <item>checkpoint</item>
            <item>save</item>
            <item>sleep</item>
            <item>exists</item>
            <item>congr</item>
            <item>simp</item>

            <item>introv</item>
            <item>transitivity</item>
            <item>use</item>
            <item>inductive'</item>
            <item>rcases</item>
            <item>obtain</item>
            <item>by_cases</item>
            <item>simpa</item>
            <item>cases'</item>
            <item>contrapose!</item>
            <item>contrapose</item>
            <item>rwa</item>
            <item>ext</item>
            <item>push_neg</item>
            <item>nth_rw</item>
            <item>by_contra</item>
            <item>by_contra'</item>
            <item>conv_rhs</item>
            <item>conv_lhs</item>
            <item>linarith</item>
            <item>library_search</item>
            <item>rintro</item>
            <item>rintros</item>
            <item>choose</item>
            <item>ring</item>
            <item>exfalso</item>
            <item>by_contradiction</item>
            <item>by_contradiction'</item>
            <item>left</item>
            <item>right</item>
            <item>refl</item>

            <item>from</item>
            <item>using</item>
        </list>
        <!-- { "begin": "«", "end": "»", "contentName": "entity.name.lean4" }, -->
        <!-- {
            "begin": "\"", "end": "\"",
            "name": "string.quoted.double.lean4",
            "patterns": [
              { "match": "\\\\[\\\\\"ntr']", "name": "constant.character.escape.lean4" },
              { "match": "\\\\x[0-9A-Fa-f][0-9A-Fa-f]", "name": "constant.character.escape.lean4" },
              { "match": "\\\\u[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]", "name":
        "constant.character.escape.lean4" }
            ]
          }, -->
        <!-- { "name": "constant.language.lean4", "match": "\\b(true|false)\\b" }, -->
        <list name="Constants"> <!-- dsConstant -->
            <item>true</item>
            <item>false</item>
        </list>
        <!-- TODO { "name": "string.quoted.single.lean4", "match": "'[^\\\\']'" },
          { "name": "string.quoted.single.lean4", "match":
        "'(\\\\(x[0-9A-Fa-f][0-9A-Fa-f]|u[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]|.))'",
            "captures": { "1": { "name": "constant.character.escape.lean4" } } },
          { "match": "`+[^\\[(]\\S+", "name": "entity.name.lean4" },-->
        <!-- { "match":
        "\\b([0-9]+|0([xX][0-9a-fA-F]+)|[-]?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?)\\b",
        "name": "constant.numeric.lean4" } -->
        <!-- ], -->
        <!--"repository":
        {
          "definitionName": {
            "patterns": [
              {"match": "\\b[^:«»\\(\\)\\{\\}[:space:]=→λ∀?][^:«»\\(\\)\\{\\}[:space:]]*", "name":
        "entity.name.function.lean4"},
              {"begin": "«", "end": "»", "contentName": "entity.name.function.lean4"}
            ]
          },
          "dashComment": {
            "begin": "--><!--", "end": "$",
            "name": "comment.line.double-dash.lean4",
            "patterns": [
              { "include": "source.lean.markdown" }
            ]
          },
          "docComment": {
            "begin": "/--><!--", "end": "-/", "name": "comment.block.documentation.lean4",
            "patterns": [
              { "include": "source.lean.markdown" },
              { "include": "#blockComment" }
            ]
          },
          "modDocComment": {
            "begin": "/-!", "end": "-/", "name": "comment.block.documentation.lean4",
            "patterns": [
              { "include": "source.lean.markdown" },
              { "include": "#blockComment" }
            ]
          },
          "blockComment": {
            "begin": "/-", "end": "-/", "name": "comment.block.lean4",
            "patterns": [
              { "include": "source.lean.markdown" },
              { "include": "#blockComment" }
            ]
          },
          "comments": {
            "patterns": [
              { "include": "#dashComment" },
              { "include": "#docComment" },
              { "include": "#stringBlock" },
              { "include": "#modDocComment" },
              { "include": "#blockComment" }
            ]
          }
        }
        } -->


        <contexts>
            <context attribute="Normal Text" lineEndContext="#stay" name="Normal Text">
                <keyword attribute="Types" context="#stay" String="Types" />
                <keyword attribute="Modifiers" context="#stay" String="Modifiers" />
                <RegExpr attribute="Modifiers" context="#stay" String="(?:attribute\b\s*|@)\[[^\]]*\]" />
                <keyword attribute="sorry" context="#stay" String="sorry" />
                <keyword attribute="Commands" context="#stay" String="Commands" />
                <keyword attribute="import" context="Importing" String="import" />
                <keyword attribute="ControlFlows" context="#stay" String="ControlFlows" />
                <keyword attribute="Definitions" context="defining" String="Definitions" />
                <keyword attribute="Tactics" context="#stay" String="Tactics" />
                <keyword attribute="Keywords" context="#stay" String="Keywords" />
                <keyword attribute="Constants" context="#stay" String="Constants" />

                <RangeDetect attribute="NameEscape" context="#stay" char="«" char1="»" />
                <RegExpr attribute="Name" context="#stay"
                    String="[A-Za-z_&#x03b1;-&#x03ba;&#x03bc;-&#x03fb;&#x1f00;-&#x1ffe;&#x2100;-&#x214f;][A-Za-z_'&#x03b1;-&#x03ba;&#x03bc;-&#x03fb;&#x1f00;-&#x1ffe;&#x2070;-&#x2079;&#x207f;-&#x2089;&#x2090;-&#x209c;&#x2100;-&#x214f;0-9]*" />
                <!--<RegExpr attribute="Hexadecimal" context="#stay" String="0[xX][0-9a-fA-F]+" />
                <RegExpr attribute="Float" context="#stay" String="[0-9]+(?:(\.[0-9]*)?[eE][+-]?[0-9]+|\.[0-9]*)" />
                <RegExpr attribute="Decimal" context="#stay" String="[0-9]+" />-->
                <DetectChar attribute="String" context="string" char="&quot;" />
                <StringDetect attribute="DocComment" context="DocMultiLineComment" String="/-!" />
                <StringDetect attribute="DocComment" context="DocMultiLineComment" String="/--" />
                <StringDetect attribute="Comment" context="#stay" String="/--/" />
                <StringDetect attribute="Comment" context="MultiLineComment" String="/-" />
                <StringDetect lookAhead="true" context="SingleLineComment" String="--" />


                <!-- <StringDetect attribute="Operation" context="#stay" String="..." />
                <StringDetect attribute="Operation" context="#stay" String="<;>" />
                <StringDetect attribute="Operation" context="#stay" String="|-" />
                <StringDetect attribute="Operation" context="#stay" String=":=" />
                <StringDetect attribute="Operation" context="#stay" String="!=" />
                <StringDetect attribute="Operation" context="#stay" String="#&amp;" />
                <StringDetect attribute="Operation" context="#stay" String="-." />
                <StringDetect attribute="Operation" context="#stay" String="->" />
                <StringDetect attribute="Operation" context="#stay" String=".." />
                <StringDetect attribute="Operation" context="#stay" String="::" />
                <StringDetect attribute="Operation" context="#stay" String=":>" />
                <StringDetect attribute="Operation" context="#stay" String=";;" />
                <StringDetect attribute="Operation" context="#stay" String="&lt;-" />
                <StringDetect attribute="Operation" context="#stay" String="==" />
                <StringDetect attribute="Operation" context="#stay" String="||" />
                <StringDetect attribute="Operation" context="#stay" String="&lt;-" />
                <StringDetect attribute="Operation" context="#stay" String="=>" />
                <StringDetect attribute="Operation" context="#stay" String="&lt;=" />
                <StringDetect attribute="Operation" context="#stay" String=">=" />
                <StringDetect attribute="Operation" context="#stay" String="/\" />
                <StringDetect attribute="Operation" context="#stay" String="\/" />
                <StringDetect attribute="Operation" context="#stay" String="⁻¹" />
                <AnyChar attribute="Operation" context="#stay"
                String="#&amp;*+-/@!`.;&lt;=>_|~∀Πλ↔∧∨≠≤≥¬⬝▸←→∃≈×⌞⌟≡⟨⟩⊢‹›" />
                <AnyChar attribute="Constants" context="#stay" String="ℕℤ" /> -->
            </context>

            <context attribute="Comment" lineEndContext="#pop" name="SingleLineComment">
            </context>

            <context attribute="Comment" lineEndContext="#stay" name="MultiLineComment">
                <StringDetect attribute="Comment" context="#pop" String="-/" />
                <StringDetect attribute="Comment" context="#stay" String="/--/" />
                <StringDetect attribute="DocComment" context="DocMultiLineComment" String="/-!" />
                <StringDetect attribute="DocComment" context="DocMultiLineComment" String="/--" />
                <StringDetect attribute="Comment" context="MultiLineComment" String="/-" />
            </context>

            <context attribute="DocComment" lineEndContext="#stay" name="DocMultiLineComment">
                <StringDetect attribute="DocComment" context="#pop" String="-/" />
                <StringDetect attribute="Comment" context="#stay" String="/--/" />
                <StringDetect attribute="DocComment" context="DocMultiLineComment" String="/-!" />
                <StringDetect attribute="DocComment" context="DocMultiLineComment" String="/--" />
                <StringDetect attribute="Comment" context="MultiLineComment" String="/-" />
            </context>

            <context attribute="String" lineEndContext="#stay" name="string">
                <DetectChar attribute="String" context="#pop" char="&quot;" />
                <RegExpr attribute="StringEscape" context="#stay"
                    String="\\(?:[\\&quot;ntr']|x[0-9A-Fa-f]{2}|u[0-9A-Fa-f]{4})" />
                <DetectChar attribute="ErrorStringEscape" context="#stay" char="\" />
            </context>

            <context attribute="Importing" lineEndContext="#pop" name="Importing">
                <StringDetect attribute="Comment" context="#stay" String="/--/" />
                <StringDetect attribute="Comment" context="MultiLineComment" String="/-" />
                <StringDetect lookAhead="true" context="SingleLineComment" String="--" />
            </context>

            <context attribute="Defining" lineEndContext="#stay" name="defining">
                <keyword attribute="DefinitionsAfterName" context="#pop"
                    String="DefinitionsAfterName" />
                <AnyChar attribute="String" context="#pop" String="&quot;:|([{⦃&lt;>"
                    lookAhead="true" />
                <StringDetect attribute="Comment" context="#stay" String="/--/" />
                <StringDetect attribute="Comment" context="MultiLineComment" String="/-" />
                <StringDetect lookAhead="true" context="SingleLineComment" String="--" />
            </context>
        </contexts>

        <itemDatas>
            <itemData name="Normal Text" defStyleNum="dsNormal" spellChecking="false" />
            <itemData name="Types" defStyleNum="dsDataType" spellChecking="false" />
            <itemData name="Modifiers" defStyleNum="dsAttribute" spellChecking="false" />
            <itemData name="sorry" defStyleNum="dsError" spellChecking="false" />
            <itemData name="Commands" defStyleNum="dsPreprocessor" spellChecking="false" />
            <itemData name="import" defStyleNum="dsKeyword" spellChecking="false" />
            <itemData name="ControlFlows" defStyleNum="dsControlFlow" spellChecking="false" />
            <itemData name="Definitions" defStyleNum="dsKeyword" spellChecking="false" />
            <itemData name="DefinitionsAfterName" defStyleNum="dsKeyword" spellChecking="false" />

            <itemData name="Operation" defStyleNum="dsOperator" spellChecking="false"/>

            <itemData name="Tactics" defStyleNum="dsKeyword" spellChecking="false" />
            <itemData name="Keywords" defStyleNum="dsKeyword" spellChecking="false" />
            <itemData name="Constants" defStyleNum="dsConstant" spellChecking="false" />

            <itemData name="NameEscape" defStyleNum="dsNormal" spellChecking="false" />
            <itemData name="Name" defStyleNum="dsNormal" spellChecking="false" />
            <!--<itemData name="Hexadecimal" defStyleNum="dsBaseN" spellChecking="false" />
            <itemData name="Float" defStyleNum="dsFloat" spellChecking="false" />
            <itemData name="Decimal" defStyleNum="dsDecVal" spellChecking="false" />-->

            <itemData name="Defining" defStyleNum="dsFunction" spellChecking="false" />
            <itemData name="Importing" defStyleNum="dsImport" spellChecking="false" />
            <itemData name="String" defStyleNum="dsString" spellChecking="false" />
            <itemData name="StringEscape" defStyleNum="dsSpecialChar" spellChecking="false" />
            <itemData name="ErrorStringEscape" defStyleNum="dsError" spellChecking="false" />
            <itemData name="Comment" defStyleNum="dsComment" />
            <itemData name="DocComment" defStyleNum="dsDocumentation" />
        </itemDatas>
    </highlighting>
    <general>
        <comments>
            <comment name="singleLine" start="--" position="afterwhitespace" />
            <comment name="multiLine" start="/-" end="-/" />
        </comments>
        <keywords casesensitive="1" weakDeliminator="#._"
            additionalDeliminator="#&amp;*+-/!`;&lt;=>_|~∀Πλ↔∧∨≠≤≥¬·⬝▸←→∃≈×⌞⌟≡⟨⟩⊢‹›⦃⦄" />
        <folding indentationsensitive="1" />
    </general>
</language>
|]