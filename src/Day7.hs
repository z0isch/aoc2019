module Day7 where

import Relude

import Day5 (dayInput, runProg, step, ProgError(..), Prog(..), ProgStatus(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad (foldM)
import Data.List (maximum)
import Control.Monad.Loops (iterateUntilM)
import Data.Sequence (Seq(..), (|>), (<|))
import qualified Data.Sequence as S

-- brittany-disable-next-binding
test :: Map Integer Integer
test = M.fromList $ zip [0..] [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

runSequenced :: Prog -> Prog -> Either ProgError Prog
runSequenced p1 p2@(Prog { input }) =
  runProg p1 <&> \(Prog { output }) -> p2 { input = input ++ output }

runWithPhaseSettings
  :: Map Integer Integer -> [Integer] -> Either ProgError Prog
runWithPhaseSettings _ [] = Left NoMoreInput
runWithPhaseSettings inst (p : ps) =
  foldM runSequenced (Prog Ready [p, 0] [] 0 0 inst) progs >>= runProg
  where progs = fmap (\px -> Prog Ready [px] [] 0 0 inst) ps

part1 :: Map Integer Integer -> Integer
part1 inst = maximum $ catMaybes $ rights $ map
  (fmap (fmap head . nonEmpty . output) . (runWithPhaseSettings inst))
  (permutations [0 .. 4])

part1Sol :: IO Integer
part1Sol = part1 <$> dayInput "./data/day7.txt"

iterateUntiAwaitingInputOrHalt :: Prog -> (Either ProgError Prog)
iterateUntiAwaitingInputOrHalt p =
  step p
    >>= iterateUntilM ((\s -> s == Halted || s == AwaitingInput) . status) step

part2Loop :: State (Seq Prog) (Either ProgError Prog)
part2Loop = get >>= \case
  p1 :<| p2@(Prog { input }) :<| ps ->
    case step p1 >>= iterateUntiAwaitingInputOrHalt of
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

runPart2 :: Map Integer Integer -> [Integer] -> Either ProgError Prog
runPart2 _ [] = Left NoMoreInput
runPart2 inst (p : ps) = evalState
  part2Loop
  (S.fromList
  $ Prog Ready [p, 0] [] 0 0 inst
  : fmap (\px -> Prog Ready [px] [] 0 0 inst) ps
  )

part2 :: Map Integer Integer -> Integer
part2 inst = maximum $ catMaybes $ rights $ map
  (fmap (fmap head . nonEmpty . output) . (runPart2 inst))
  (permutations [5 .. 9])

part2Sol :: IO Integer
part2Sol = part2 <$> dayInput "./data/day7.txt"
