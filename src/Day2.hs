module Day2 where

import Relude hiding (head, last)

import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as V
import Relude.Unsafe (head, last)

type Prog = V.Vector Int

input :: IO Prog
input = V.fromList . map (fromJust . readMaybe) . sepBy ',' <$> readFile
  "data/day2.txt"

sepBy :: Char -> String -> [String]
sepBy _ [] = []
sepBy c s = cons $ case break (== c) s of
  (l, s') ->
    ( l
    , case s' of
      [] -> []
      _ : s'' -> sepBy c s''
    )
  where cons ~(h, t) = h : t

op :: Int -> Maybe (Int -> Int -> Int)
op 1 = Just (+)
op 2 = Just (*)
op _ = Nothing

runOp :: (Prog, Int) -> Maybe (Prog, Int)
runOp (_, 99) = Nothing
runOp (v, i) = do
  o <- v V.!? i >>= op
  n <- o <$> a <*> b
  s <- v V.!? (i + 3)
  pure ((v V.// [(s, n)]), i + 4)
 where
  a = idx (i + 1)
  b = idx (i + 2)
  idx x = v V.!? x >>= (V.!?) v

runProg :: Int -> Int -> Prog -> Int
runProg noun verb p = V.head $ last $ go
  (Just (p V.// [(1, noun), (2, verb)], 0))
 where
  go Nothing = []
  go (Just (p, i)) = p : go (runOp (p, i))

part1 :: IO Int
part1 = runProg 12 2 <$> input

allCombinations :: Prog -> [(Int, Int, Int)]
allCombinations p = [ (n, v, runProg n v p) | n <- [0 ..], v <- [0 .. n] ]

part2 :: IO Int
part2 =
  ((\(n, v, _) -> n * 100 + v)
    . head
    . dropWhile (\(_, _, s) -> s /= 19690720)
    . allCombinations
    )
    <$> input
