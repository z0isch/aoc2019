module Day1 where

import           RIO

input :: IO [Integer]
input = map  read . lines <$> readFile "data/day1.txt"

part1 :: IO Integer
part1 = sum . map fuel <$> input

part2 :: IO Integer
part2 = sum . concatMap (tail . takeWhile (> 0) . iterate fuel) <$> input

fuel :: Integer -> Integer
fuel mass = (mass `div` 3) - 2

