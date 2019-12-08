module Day8 where

import Relude

import Data.List (minimumBy)
import Codec.Picture (generateImage, Image(..), PixelRGB8(..), writeBitmap)
import qualified Data.Vector as V
import Data.Vector ((!))
import Control.Arrow

test :: String
test = "0222112222120000"

layerize :: Int -> Int -> String -> [[String]]
layerize wide tall = go
 where
  go [] = []
  go xs = getLayer tall xs : go (drop (wide * tall) xs)
  getLayer 0 _ = []
  getLayer n xs = take wide xs : (getLayer (n - 1) (drop wide xs))

checksum :: [[String]] -> Product Int
checksum =
  bifoldMap (Product . getSum) (Product . getSum)
    . foldMap (numOf '1' &&& numOf '2')
    . minimumBy (compare `on` (foldMap (numOf '0')))
  where numOf x = Sum . length . (filter (== x))

part1 :: IO (Product Int)
part1 = checksum . layerize 25 6 <$> readFile "./data/day8.txt"

mkImage :: Int -> Int -> String -> Image PixelRGB8
mkImage wide tall xs = generateImage
  (\x y -> pixelColor (fmap (\vs -> vs ! y ! x) layers))
  wide
  tall
  where layers = fmap (V.fromList . fmap V.fromList) $ layerize wide tall xs

pixelColor :: String -> PixelRGB8
pixelColor = fromMaybe white . fmap (mkPixel . head) . nonEmpty . filter
  (/= '2')
 where
  mkPixel '0' = black
  mkPixel _ = white
  black = PixelRGB8 0 0 0
  white = PixelRGB8 255 255 255

part2 :: IO ()
part2 = writeBitmap "./output/day8-part2.bmp" . mkImage 25 6 =<< readFile
  "./data/day8.txt"
