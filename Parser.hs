module Parser
( Flag
, flagSingleton
, operation
, imageContentsToList
, getImageHeaders
, getImagePixels
)

where

import Pixmap
import Data.String
import Data.List

data Flag = FH | FV | HW | HH | GS | RC | GC | BC

flagSingleton :: String -> Flag
flagSingleton "fh" = FH
flagSingleton "fv" = FV
flagSingleton "hw" = HW
flagSingleton "hh" = HH
flagSingleton "gs" = GS
flagSingleton "rc" = RC
flagSingleton "gc" = GC
flagSingleton "bc" = BC

operation :: Flag -> (Image -> Image)
operation FH = flipHorizontal
operation FV = flipVertical
operation HW = halveWidth
operation HH = halveHeight
operation GS = grayscaleFilter
operation RC = redFilter
operation GC = greenFilter
operation BC = blueFilter

imageContentsToList :: String -> [Int]
imageContentsToList = map (read) . tail . words . unlines . filterComments . lines

getImageHeaders :: [Int] -> [Int]
getImageHeaders xs = take 3 xs

getImagePixels :: Int -> [Int] -> [[(Int, Int, Int)]]
getImagePixels width xs = map (\ys -> map (\(p1:p2:p3:_) -> (p1, p2, p3)) ys) $ split width $ split 3 $ drop 3 xs

filterComments :: [String] -> [String]
filterComments xs = filter (not . isPrefixOf "#") xs
