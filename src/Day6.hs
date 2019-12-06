module Day6 where

import Relude

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (splitOn)
import Data.List (minimum)

test =
  mkOrbitMap
    $ fmap ((\[a, b] -> (a, b)) . splitOn ")")
    $ lines
    $ "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"


input :: IO [(Text, Text)]
input = fmap ((\[a, b] -> (a, b)) . splitOn ")") . lines <$> readFileText
  "data/day6.txt"

mkOrbitMap :: [(Text, Text)] -> Map Text (Set Text)
mkOrbitMap = foldr
  (\(orbitee, orbiter) -> M.insertWith S.union orbitee (S.singleton orbiter))
  mempty

countOrbits :: Map Text (Set Text) -> Text -> Sum Int
countOrbits orbits orbitee =
  maybe
      mempty
      (\orbiters ->
        Sum (S.size orbiters) <> foldMap (countOrbits orbits) orbiters
      )
    $ M.lookup orbitee orbits

getParent :: Text -> Map Text (Set Text) -> Maybe Text
getParent me = fmap head . nonEmpty . M.keys . M.filter (me `S.member`)

isDownStream :: Map Text (Set Text) -> Text -> Text -> Any
isDownStream orbits me you =
  maybe
      mempty
      (\children ->
        Any (me `S.member` children)
          <> foldMap (isDownStream orbits me) children
      )
    $ M.lookup you orbits

steps :: Map Text (Set Text) -> Text -> Text -> Maybe Int
steps orbits parent me = go 0 parent
 where
  go steps parent' =
    maybe
        Nothing
        (\children -> if me `S.member` children
          then Just (steps + 1)
          else fmap head $ nonEmpty $ mapMaybe (go (steps + 1)) $ S.toList
            children
        )
      $ M.lookup parent' orbits


stepsBetween :: Text -> Text -> Map Text (Set Text) -> [Int]
stepsBetween me you orbits = mapMaybe
  (\p -> (+) <$> steps orbits p me <*> steps orbits p you)
  parentsOfBoth
 where
  parentsOfBoth =
    filter
        (\orbitee -> getAny (isDownStream orbits me orbitee)
          && getAny (isDownStream orbits you orbitee)
        )
      $ M.keys orbits

part1 :: IO (Sum Int)
part1 = do
  orbits <- mkOrbitMap <$> input
  pure $ foldMap (countOrbits orbits) $ M.keys orbits

part2 :: IO Int
part2 =
  (\s -> s - 2) . minimum . (stepsBetween "YOU" "SAN") . mkOrbitMap <$> input


