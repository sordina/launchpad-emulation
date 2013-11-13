module Algebra where

import Data.List.Split
import Data.List
import Data.Char
import Data.Maybe
import Safe

interpret :: String -> Int -> Int -> Int
interpret s x y = addFun $ substitute "x" (show x) $ substitute "y" (show y) $ s

substitute :: String -> String -> String -> String
substitute var val s = intercalate val $ splitOn var s

addFun :: String -> Int
addFun = sum . map timesFun . splitOn "+"

timesFun :: String -> Int
timesFun s = if elem '*' s then product $ map literalFun $ splitOn "*" s
                               else (literalFun s)

literalFun :: String -> Int
literalFun s = fromMaybe 42 $ readMay (filter isDigit s)
