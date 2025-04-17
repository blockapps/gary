
module Text.Tools
  ( box,
    boringBox,
    formatBool,
    grayBox,
    setTitle,
    shorten,
    tab,
    tab',
    wrap,
    showRanges
  )
where

import Control.Monad
import Data.Char
import qualified Data.Text as T
import Text.Colors as C

box :: [String] -> String
box = boxWithProperty C.magenta

grayBox :: [String] -> String
grayBox = boxWithProperty (C.dim . C.white)

boxWithProperty :: (String -> String) -> [String] -> String
boxWithProperty property strings =
  unlines $
    [property ("╔" ++ replicate (width - 2) '═' ++ "╗")]
      ++ map (\s -> property "║ " ++ C.black s ++ replicate (width - printedLength s - 4) ' ' ++ property " ║") strings
      ++ [property ("╚" ++ replicate (width - 2) '═' ++ "╝")]
  where
    width = maximum (map printedLength strings) + 4

printedLength :: String -> Int
printedLength = go False
  where
    go :: Bool -> String -> Int
    go True ('m' : t) = go False t
    go True (_ : t) = go True t
    go False ('\ESC' : t) = go True t
    go False (c : t) | generalCategory c == OtherSymbol= 2 + go False t
    go False (_ : t) = 1 + go False t
    go _ [] = 0

boringBox :: [String] -> String
boringBox [] = ""
boringBox strings =
  unlines $
    [C.magenta (replicate width '=')]
      ++ map (\s -> C.magenta "| " ++ C.white s ++ replicate (width - printedLength s - 4) ' ' ++ C.magenta " |") strings
      ++ [C.magenta (replicate width '=')]
  where
    width = maximum (map printedLength strings) + 4

formatBool :: Bool -> String
formatBool True = C.green "True"
formatBool False = C.red "False"

tab :: String -> String
tab s = ' ' : ' ' : ' ' : ' ' : tab' s

-- This is a second version of "tab" that skips the first line
tab' :: String -> String
tab' [] = []
tab' ('\n' : rest) = '\n' : ' ' : ' ' : ' ' : ' ' : tab' rest
tab' (c : rest) = c : tab' rest

setTitle :: String -> IO ()
setTitle value = do
  putStr $ "\ESC]0;" ++ value ++ "\007"

shorten :: Int -> String -> String
shorten maxLen s | length s <= maxLen = s
shorten maxLen s = take maxLen s ++ "..."

wrap :: Int -> String -> [String]
wrap maxLen s | length s > maxLen = take maxLen s : wrap maxLen (drop maxLen s)
wrap _ s = [s]

showRanges :: [Integer] -> String
showRanges [] = "<empty list>"
showRanges [v] = "(" ++ show v ++ ")"
showRanges (v1:rest) = "(" ++ show v1 ++ showRangeEnding rest

showRangeEnding :: [Integer] -> String
showRangeEnding [] = "<empty list>"
showRangeEnding [v] = ":" ++ show v ++ ")"
showRangeEnding (v1:v2:rest) | v2 == v1 + 1 = showRangeEnding (v2:rest)
showRangeEnding (v1:v2:rest) = ":" ++ show v1 ++ "), " ++ showRanges (v2:rest)

