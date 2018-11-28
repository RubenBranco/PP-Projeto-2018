import System.IO
import System.Environment
import Parser
import Pixmap

main = do
    (imagePath:newImagePath:flags) <- getArgs
    imageContents <- readFile imagePath
    let imageList = imageContentsToList imageContents
    let headers = getImageHeaders imageList
    let pixels = getImagePixels (head headers) imageList
    let image = fromFileContents (head headers) (headers !! 1) (headers !! 2) pixels
    let newImage = foldl (\acc f -> (operation f) acc) image $ map (\(_:flag) -> flagSingleton flag) flags
    writeFile newImagePath $ unlines ["P3", show newImage]
    return ()


