module Day1 where

import Relude hiding (tail)

import Data.Maybe (fromJust)
import Data.Monoid
import Data.Text (unpack)
import Relude.Unsafe (tail)

input :: IO [Integer]
input =
  map (fromJust . readMaybe . unpack) . lines <$> readFileText "data/day1.txt"

part1 :: IO Integer
part1 = getSum . foldMap (Sum . fuel) <$> input

part2 :: IO Integer
part2 = sum . concatMap (takeWhile (> 0) . tail . iterate fuel) <$> input

fuel :: Integer -> Integer
fuel mass = (mass `div` 3) - 2

