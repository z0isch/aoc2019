module Day9 where

import Relude

import Day5 (Prog(..), ProgError, ProgStatus(..), dayInput, runProg)

part1 :: IO (Either ProgError Prog)
part1 = runProg . Prog Ready [1] [] 0 0 <$> dayInput "./data/day9.txt"
part2 :: IO (Either Day5.ProgError Prog)
part2 = runProg . Prog Ready [2] [] 0 0 <$> dayInput "./data/day9.txt"
