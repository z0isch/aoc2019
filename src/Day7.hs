module Day7 where

import Relude

import Day5 (dayInput, runProg, step, ProgError(..), Prog(..), ProgStatus(..))
import qualified Data.Vector as V
import Data.Vector (Vector)
import Control.Monad (foldM)
import Data.List (maximum)
import Control.Monad.Loops (iterateUntilM)
import Data.Sequence (Seq(..), (|>), (<|))
import qualified Data.Sequence as S

-- brittany-disable-next-binding
test :: Vector Int
test = V.fromList [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

runSequenced :: Prog -> Prog -> Either ProgError Prog
runSequenced p1 p2@(Prog { input }) =
  runProg p1 <&> \(Prog { output }) -> p2 { input = input ++ output }

runWithPhaseSettings :: Vector Int -> [Int] -> Either ProgError Prog
runWithPhaseSettings _ [] = Left NoMoreInput
runWithPhaseSettings inst (p : ps) =
  foldM runSequenced (Prog Ready [p, 0] [] 0 inst) progs >>= runProg
  where progs = fmap (\px -> Prog Ready [px] [] 0 inst) ps

allPhaseSettings :: Int -> Int -> [[Int]]
allPhaseSettings x y =
  let allSettings = [x .. y]
  in
    [ [p1, p2, p3, p4, p5]
    | p1 <- allSettings
    , p2 <- filter (`notElem` [p1]) allSettings
    , p3 <- filter (`notElem` [p1, p2]) allSettings
    , p4 <- filter (`notElem` [p1, p2, p3]) allSettings
    , p5 <- filter (`notElem` [p1, p2, p3, p4]) allSettings
    ]

part1 :: Vector Int -> Int
part1 inst = maximum $ catMaybes $ rights $ map
  (fmap (fmap head . nonEmpty . output) . (runWithPhaseSettings inst))
  (allPhaseSettings 0 4)

part1Sol :: IO Int
part1Sol = part1 <$> dayInput "./data/day7.txt"

iterateUntiAwaitingInputOrHalt :: Prog -> (Either ProgError Prog)
iterateUntiAwaitingInputOrHalt p =
  step p
    >>= iterateUntilM ((\s -> s == Halted || s == AwaitingInput) . status) step

part2Loop :: State (Seq Prog) (Either ProgError Prog)
part2Loop = get >>= \case
  p1 :<| p2@(Prog { input }) :<| ps ->
    case iterateUntiAwaitingInputOrHalt p1 of
      Left e -> pure $ Left e
      Right p1'@(Prog { output }) -> case status p2 of
        Halted -> pure $ Right p1'
        _ -> do
          let
            p1'' = p1' { output = [] }
            p2' = p2 { input = input ++ output }
            ps' = (p2' <| ps) |> p1''
          put ps'
          part2Loop
  _ -> pure $ Left NoMoreInput

runPart2 :: Vector Int -> [Int] -> Either ProgError Prog
runPart2 _ [] = Left NoMoreInput
runPart2 inst (p : ps) = evalState
  part2Loop
  (S.fromList
  $ Prog Ready [p, 0] [] 0 inst
  : fmap (\px -> Prog Ready [px] [] 0 inst) ps
  )

part2 :: Vector Int -> Int
part2 inst = maximum $ catMaybes $ rights $ map
  (fmap (fmap head . nonEmpty . output) . (runPart2 inst))
  (allPhaseSettings 5 9)

part2Sol :: IO Int
part2Sol = part2 <$> dayInput "./data/day7.txt"
