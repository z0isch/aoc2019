module Day14 where

import Relude hiding (some)

import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import qualified Data.Map.Strict as M
import Control.Lens

test, test2 :: Reactions
test = Unsafe.fromJust $ parseMaybe
  inputP
  "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL"
test2 = Unsafe.fromJust $ parseMaybe
  inputP
  "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"

type Chemical = String
type Reaction = ((Integer, Chemical), [(Integer, Chemical)])
type Reactions = Map Chemical (Integer, [(Integer, Chemical)])

type Parser = Parsec Void Text

spaceL :: Parser a -> Parser a
spaceL = lexeme space1

chemicalP :: Parser (Integer, Chemical)
chemicalP = (,) <$> (spaceL decimal) <*> (some letterChar)

reactionP :: Parser Reaction
reactionP =
  flip (,)
    <$> spaceL (chemicalP `sepBy1` (spaceL (char ',')))
    <*> (spaceL (string "=>") *> chemicalP)

inputP :: Parser Reactions
inputP =
  M.fromList
    . fmap (\((n, k), xs) -> (k, (n, xs)))
    <$> reactionP
    `sepBy1` newline

input :: IO (Maybe Reactions)
input = parseMaybe inputP <$> readFileText "data/day14.txt"

neededOre :: Reactions -> Sum Integer
neededOre reactions = (M.! "ORE") $ fst $ go mempty (mempty, [(1, "FUEL")])
 where
  go _ (reqs, []) = (reqs, [])
  go extras (reqs, ((numOre, "ORE") : cs)) =
    go extras (M.insertWith (<>) "ORE" (Sum numOre) reqs, cs)
  go extras (reqs, ((numC, c) : cs)) =
    let
      (extras', chemicalsToProduce) =
        runReverseReaction reactions extras (numC, c)
    in
      go extras' (M.insertWith (<>) c (Sum numC) reqs, cs ++ chemicalsToProduce)

runReverseReaction
  :: Reactions
  -> Map Chemical (Sum Integer)
  -> (Integer, Chemical)
  -> (Map Chemical (Sum Integer), [(Integer, Chemical)])
runReverseReaction reactions extras (numNeededC, neededChemical) =
  let
    extrasOfNeededChemical = fromMaybe 0 $ M.lookup neededChemical extras
    numNeededAfterExtras = numNeededC - getSum extrasOfNeededChemical
    (numProducedC, chemicalsToProduce) = reactions M.! neededChemical
    (timesRun, leftOvers) =
      (\(d, m) ->
          let run = if m == 0 then d else d + 1
          in (run, (run * numProducedC) - numNeededAfterExtras)
        )
        $ numNeededAfterExtras
        `divMod` numProducedC
  in if numNeededAfterExtras <= 0
    then (M.insert neededChemical (Sum $ abs numNeededAfterExtras) extras, [])
    else
      ( M.insert neededChemical (Sum leftOvers) extras
      , over _1 (* timesRun) <$> chemicalsToProduce
      )

part1 :: IO (Maybe (Sum Integer))
part1 = fmap neededOre <$> input
