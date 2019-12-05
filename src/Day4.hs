module Day4 where

import Relude

type Pass = [Int]

input :: [Pass]
input = map mkPass [146810 .. 612564]

mkPass :: Integer -> [Int]
mkPass i = mapMaybe (readMaybe . pure) $ show i

isValid :: Pass -> Bool
isValid xs = sort xs == xs && not (null (filter ((> 1) . length) (group xs)))

part1 :: Int
part1 = length $ filter isValid input

isValid2 :: Pass -> Bool
isValid2 xs = sort xs == xs && not (null (filter ((== 2) . length) (group xs)))

part2 :: Int
part2 = length $ filter isValid2 input
