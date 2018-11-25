-- RGB (Red Green Blue)
module Pixmap
( Image
, fromFileContents
, flipVertical
, flipHorizontal
, redFilter
, greenFilter
, blueFilter
)

where
data Image = Pixel Int Int Int | P3 Int Int Int [[Image]]

instance Show Image where
    show (Pixel r g b) = show r ++ " " ++ show g ++ " " ++ show b
    show (P3 h w maxV pixels) = show h ++ " " ++ show w ++ " " ++ show maxV ++ (foldl (\acc x -> acc ++ pixelRowShow x) "" pixels)

pixelRowShow :: [Image] -> String
pixelRowShow = foldl (\acc x -> acc ++ " " ++ show x) ""

fromFileContents :: Int -> Int -> Int -> [[(Int, Int, Int)]] -> Image
fromFileContents h w maxV pixels = P3 h w maxV (map (\x -> map (\(r, g, b) -> Pixel r g b) x) pixels)

flipVertical :: Image -> Image
flipVertical (P3 h w maxV pixels) = P3 h w maxV (reverse pixels)

flipHorizontal :: Image -> Image
flipHorizontal (P3 h w maxV pixels) = P3 h w maxV (map (reverse) pixels)

redFilter :: Image -> Image
redFilter (P3 h w maxV pixels) = P3 h w maxV (map (\x -> map (\(Pixel r _ _) -> Pixel r 0 0) x) pixels)

greenFilter :: Image -> Image
greenFilter (P3 h w maxV pixels) = P3 h w maxV (map (\x -> map (\(Pixel _ g _) -> Pixel 0 g 0) x) pixels)

blueFilter :: Image -> Image
blueFilter (P3 h w maxV pixels) = P3 h w maxV (map (\x -> map (\(Pixel _ _ b) -> Pixel 0 0 b) x) pixels)
