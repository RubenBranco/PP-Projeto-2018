import Pixmap
import Test.QuickCheck

prop_image_invert_horizontal :: Image -> Bool
prop_image_invert_horizontal img = flipHorizontal (flipHorizontal img) == img

prop_pixel_dimension :: Image -> Bool
prop_pixel_dimension img = (getImageHeight img) * (getImageWidth img) == numberOfPixels img

prop_ratio_upon_reduction :: Image -> Bool
prop_ratio_upon_reduction img = (getImageHeight img) / (getImageWidth img) == (getImageHeight reducedImg) / (getImageWidth reducedImg)
                                where reducedImg = halveWidth $ halveHeight img

