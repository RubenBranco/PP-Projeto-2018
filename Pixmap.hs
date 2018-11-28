-- RGB (Red Green Blue)
module Pixmap
( Image
, fromFileContents
, flipVertical
, flipHorizontal
, redFilter
, greenFilter
, blueFilter
, halveHeight
, halveWidth
, grayscaleFilter
, split
)

where
import Data.List
data Image = Pixel Int Int Int | P3 Int Int Int [[Image]]

instance Show Image where
    show (Pixel r g b) = show r ++ " " ++ show g ++ " " ++ show b
    show (P3 h w maxV pixels) = show w ++ " " ++ show h ++ " " ++ show maxV ++ (foldl (\acc x -> acc ++ pixelRowShow x) "" pixels)

pixelRowShow :: [Image] -> String
pixelRowShow = foldl (\acc x -> acc ++ " " ++ show x) ""

fromFileContents :: Int -> Int -> Int -> [[(Int, Int, Int)]] -> Image
fromFileContents h w maxV pixels = P3 h w maxV $ map (\x -> map (\(r, g, b) -> Pixel r g b) x) pixels

flipVertical :: Image -> Image
flipVertical (P3 h w maxV pixels) = P3 h w maxV $ reverse pixels

flipHorizontal :: Image -> Image
flipHorizontal (P3 h w maxV pixels) = P3 h w maxV $ map (reverse) pixels

redFilter :: Image -> Image
redFilter (P3 h w maxV pixels) = P3 h w maxV $ map (\x -> map (\(Pixel r _ _) -> Pixel r 0 0) x) pixels

greenFilter :: Image -> Image
greenFilter (P3 h w maxV pixels) = P3 h w maxV $ map (\x -> map (\(Pixel _ g _) -> Pixel 0 g 0) x) pixels

blueFilter :: Image -> Image
blueFilter (P3 h w maxV pixels) = P3 h w maxV $ map (\x -> map (\(Pixel _ _ b) -> Pixel 0 0 b) x) pixels

grayscaleFilter :: Image -> Image
grayscaleFilter (P3 h w maxV pixels) = P3 h w maxV $ map (\x -> map (\(Pixel r g b) -> grayscale (Pixel r g b)) x) pixels

grayscale :: Image -> Image
grayscale (Pixel r g b) = Pixel avg avg avg where avg = (r + b + g) `div` 3

halveHeight :: Image -> Image
halveHeight (P3 h w maxV pixels) = P3 (h `div` 2) w maxV $ transpose $ map (halvePixels) $ transpose pixels

halveWidth :: Image -> Image
halveWidth (P3 h w maxV pixels) = P3 h (w `div` 2) maxV $ map (halvePixels) pixels

halvePixels :: [Image] -> [Image]
halvePixels pixels = map (averagePixels) $ splitInTwo 2 pixels

averagePixels :: [Image] -> Image
averagePixels ((Pixel r1 g1 b1): (Pixel r2 g2 b2): _) = Pixel ((r1 + r2) `div` 2) ((g1 + g2) `div` 2) ((b1 + b2) `div` 2)
averagePixels ((Pixel r g b): _) = Pixel r g b

splitInTwo :: Int -> [a] -> [[a]]
splitInTwo _ [] = []
splitInTwo n xs
        | odd $ length xs = split n (take ((length xs) `div` 2) xs) ++ [[xs !! (length xs `div` 2)]] ++ split n (drop ((length xs) `div` 2 + 1) xs)
        | otherwise = split n xs

split :: Int -> [a] -> [[a]]
split _ [] = []
split n xs = (take n xs) : split n (drop n xs)
