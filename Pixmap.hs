-- RGB (Red Green Blue)
module Pixmap
( Image
,

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

