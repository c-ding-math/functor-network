module Parse.Svg (scale, addDepth, getDepths, getSize, stringToDouble) where

import Text.Regex.Posix
import Text.Regex (mkRegexWithOpts, subRegex)

type Svg = String
type Depth = Double

scale:: Double -> Svg -> Svg
scale x svg =  do
    let (width, height) = getSize svg
        width' = width * x
        height' = height * x
        svg' = subRegex (mkRegexWithOpts "(<svg[^>]*width=)\'[0-9]*\\.[0-9]*(pt\'[^>]*>)" False True) svg ("\\1\'"++(show width')++"\\2")
        svg'' = subRegex (mkRegexWithOpts "(<svg[^>]*height=)\'[0-9]*\\.[0-9]*(pt\'[^>]*>)" False True) svg' ("\\1\'"++(show height')++"\\2")  
    svg''

getSize:: Svg -> (Double, Double)
getSize str = do
    let widthMatches=getAllTextMatches $ str =~ "width=\'([0-9]*\\.[0-9]*)pt\'" :: [String]
        width=(map stringToDouble widthMatches)!!0
        heightMatches=getAllTextMatches $ str =~ "height=\'([0-9]*\\.[0-9]*)pt\'" :: [String]
        height=(map stringToDouble heightMatches)!!0
    (width, height)

getDepths:: String -> [Depth]
getDepths str = do
    let matches=getAllTextMatches $ str =~ "depth=([0-9]*\\.[0-9]*)pt" :: [String]
    map stringToDouble matches

addDepth:: Depth -> Svg -> Svg
addDepth depth svg = do
    let svg' = subRegex (mkRegexWithOpts "<svg(.*>.*</svg>)" False True) svg ("<svg style=\'vertical-align:-" ++ show depth ++ "pt\'\\1")
    svg'

stringToDouble:: String -> Double
stringToDouble s = do
    let doubleString = s=~"[0-9]*\\.[0-9]*"::String
        doubleString' =  if doubleString!!0 == '.' then "0" ++ doubleString else doubleString
    read doubleString' :: Double




