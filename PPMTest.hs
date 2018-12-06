module PPMTest
( prop_image_invert_horizontal
, prop_pixel_dimension
, prop_ratio_upon_reduction
, prop_max_pixel_value
, prop_image_invert_vertical
)

where

import Pixmap

prop_image_invert_horizontal :: Image -> Bool
prop_image_invert_horizontal img = flipHorizontal (flipHorizontal img) == img

prop_image_invert_vertical :: Image -> Bool
prop_image_invert_vertical img = flipVertical (flipVertical img) == img

prop_pixel_dimension :: Image -> Bool
prop_pixel_dimension img = (getImageHeight img) * (getImageWidth img) == numberOfPixels img

prop_ratio_upon_reduction :: Image -> Bool
prop_ratio_upon_reduction img = getImageHeight img `div` getImageWidth img == getImageHeight reducedImg `div` getImageWidth reducedImg
                                where reducedImg = halveWidth $ halveHeight img

prop_max_pixel_value :: Image -> Bool
prop_max_pixel_value img = length (filter (\x -> length (filter (\p -> (getRedLight p) > maxV || (getGreenLight p) > maxV || (getBlueLight p) > maxV) x) > 0) (getPixels img)) == 0
                            where maxV = getMaxV img

