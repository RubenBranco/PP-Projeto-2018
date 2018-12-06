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
, numberOfPixels
, getImageHeight
, getImageWidth
, getPixels
, getMaxV
, getRedLight
, getGreenLight
, getBlueLight
)

where
import Data.List
import Control.Monad
import Test.QuickCheck
data Image = Pixel Int Int Int | P3 Int Int Int [[Image]]

instance Show Image where
    show (Pixel r g b) = show r ++ " " ++ show g ++ " " ++ show b
    show (P3 h w maxV pixels) = show w ++ " " ++ show h ++ " " ++ show maxV ++ (foldl (\acc x -> acc ++ pixelRowShow x) "" pixels)

instance Eq Image where
    (Pixel r0 g0 b0) == (Pixel r1 g1 b1) = r0 == r1 && g0 == g1 && b0 == b1
    (P3 h0 w0 maxV0 pixels0) == (P3 h1 w1 maxV1 pixels1) = h0 == h1 && w0 == w1 && maxV0 == maxV1 && pixels0 == pixels1
    _ == _ = False

instance Arbitrary Image where
    arbitrary = do
        h <- suchThat (arbitrary :: Gen Int) (>0)
        w <- suchThat (arbitrary :: Gen Int) (>0)
        maxV <- suchThat (arbitrary :: Gen Int) (\x -> x > 0 && x <= 255)
        image <- randomImage h w maxV
        return $ P3 h w maxV image

randomImage :: Int -> Int -> Int -> Gen [[Image]]
randomImage h w maxV = vectorOf h (vectorOf w (randomPixel maxV))

randomPixel :: Int -> Gen Image
randomPixel maxV = do
        r <- choose (0, maxV) :: Gen Int
        g <- choose (0, maxV) :: Gen Int
        b <- choose (0, maxV) :: Gen Int
        return $ Pixel r g b

numberOfPixels :: Image -> Int
numberOfPixels (P3 h w maxV pixels) = foldl (\acc x -> acc + (foldl (\acc y -> acc + 1) 0 x)) 0 pixels

getImageHeight :: Image -> Int
getImageHeight (P3 h _ _ _) = h

getImageWidth :: Image -> Int
getImageWidth (P3 _ w _ _) = w

getPixels :: Image -> [[Image]]
getPixels (P3 _ _ _ pixels) = pixels

getMaxV :: Image -> Int
getMaxV (P3 _ _ maxV _) = maxV

getRedLight :: Image -> Int
getRedLight (Pixel r _ _) = r

getGreenLight :: Image -> Int
getGreenLight (Pixel _ g _) = g

getBlueLight :: Image -> Int
getBlueLight (Pixel _ _ b) = b

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
halvePixels pixels = map (averagePixels) $ filter (\x -> (length x) == 2) $ split 2 pixels

averagePixels :: [Image] -> Image
averagePixels ((Pixel r1 g1 b1): (Pixel r2 g2 b2): _) = Pixel ((r1 + r2) `div` 2) ((g1 + g2) `div` 2) ((b1 + b2) `div` 2)

split :: Int -> [a] -> [[a]]
split _ [] = []
split n xs = (take n xs) : split n (drop n xs)
