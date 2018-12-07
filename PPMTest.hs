module PPMTest
( prop_image_invert_horizontal
, prop_pixel_dimension
, prop_ratio_upon_reduction
, prop_max_pixel_value
, prop_image_invert_vertical
, prop_red_channel
, prop_green_channel
, prop_blue_channel
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

-- test whether there are positive numbers in GB values of a pixel after a red channel
prop_red_channel :: Image -> Bool
prop_red_channel img = length (filter (\x -> length (filter (\p -> (getGreenLight p) > 0 || (getBlueLight p) > 0) x) > 0) (getPixels $ redFilter img)) == 0

-- test whether there are positive numbers in RB values of a pixel after a green channel
prop_green_channel :: Image -> Bool
prop_green_channel img = length (filter (\x -> length (filter (\p -> (getRedLight p) > 0 || (getBlueLight p) > 0) x) > 0) (getPixels $ greenFilter img)) == 0

-- test whether there are positive numbers in RG values of a pixel after a blue channel
prop_blue_channel :: Image -> Bool
prop_blue_channel img = length (filter (\x -> length (filter (\p -> (getRedLight p) > 0 || (getGreenLight p) > 0) x) > 0) (getPixels $ blueFilter img)) == 0
