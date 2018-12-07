import System.IO
import System.Environment
import Test.QuickCheck
import Parser
import Pixmap
import PPMTest

main :: IO ()
main = do
    args <- getArgs
    run args

run :: [String] -> IO ()
run ["-t"] = do
        quickCheck prop_image_invert_horizontal
        quickCheck prop_pixel_dimension
        quickCheck prop_ratio_upon_reduction
        quickCheck prop_max_pixel_value
        quickCheck prop_image_invert_vertical
        quickCheck prop_red_channel
        quickCheck prop_green_channel
        quickCheck prop_blue_channel
        return ()
run (imagePath:newImagePath:flags) = do
                imageContents <- readFile imagePath
                let imageList = imageContentsToList imageContents
                let headers = getImageHeaders imageList
                let pixels = getImagePixels (head headers) imageList
                let image = fromFileContents (head headers) (headers !! 1) (headers !! 2) pixels
                let newImage = foldl (\acc f -> (operation f) acc) image $ map (\(_:flag) -> flagSingleton flag) flags
                writeFile newImagePath $ unlines ["P3", show newImage]
                return ()
