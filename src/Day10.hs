module Day10 where

import Relude

import Linear
import Data.Text (unpack)
import qualified Data.Map.Strict as M
import Data.List (maximumBy, nubBy, delete, (\\), (!!))
import Prelude (atan2)
import Linear.Plucker

-- brittany-disable-next-binding
test :: Text
test = ".#..#\n.....\n#####\n....#\n...##"

-- brittany-disable-next-binding
test2 :: Text
test2=".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##\n"

parse :: Text -> [V2 Double]
parse =
  concatMap (map snd . filter ((== '#') . fst))
    . zipWith (\y -> zipWith (\x a -> (a, V2 x y)) [0.0 ..] . unpack) [0.0 ..]
    . lines

sameAngle :: V2 Double -> V2 Double -> V2 Double -> Bool
sameAngle (V2 mx my) (V2 ax ay) (V2 cx cy) = coincides'
  (plucker3D (V3 mx my 0) (V3 ax ay 0))
  (plucker3D (V3 mx my 0) (V3 cx cy 0))

shootableAstroids :: V2 Double -> [V2 Double] -> [V2 Double]
shootableAstroids me = nubBy (sameAngle me) . sortOn (distance me)

numDetected :: [V2 Double] -> Map (V2 Double) Int
numDetected as =
  M.mapWithKey (\m -> length . shootableAstroids m)
    $ M.fromListWith (<>)
    $ [ (a1, [a2]) | a1 <- as, a2 <- as, a1 /= a2 ]

part1 :: IO (V2 Double, Int)
part1 =
  maximumBy (comparing snd) . M.toList . numDetected . parse <$> readFileText
    "./data/day10.txt"

angleBetween :: V2 Double -> V2 Double -> Double
angleBetween a b =
  let ang = atan2 (det22 (V2 a b)) (dot a b)
  in if ang < 0 then (2 * pi) + ang else ang

nthShotFrom :: Int -> V2 Double -> [V2 Double] -> V2 Double
nthShotFrom n me = go 1 . delete me
 where
  go asteroidsShot asteroids =
    let
      up = V2 0 1
      shootable =
        sortOn (angleBetween up . (me -)) $ shootableAstroids me asteroids
    in if asteroidsShot + length shootable > n
      then shootable !! (n - asteroidsShot)
      else go (asteroidsShot + length shootable) (asteroids \\ shootable)

part2 :: IO Double
part2 = do
  (bestAsteroid, _) <- part1
  V2 x y <- nthShotFrom 200 bestAsteroid . parse <$> readFileText
    "./data/day10.txt"
  pure $ x * 100 + y


