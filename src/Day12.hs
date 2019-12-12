module Day12 where

import Relude

import Linear
import Control.Lens (view, Lens')
import qualified Relude.Unsafe as Unsafe

mkMoons :: [V3 Integer] -> [Moon]
mkMoons = zipWith (Moon 0) ["Io", "Europa", "Ganymede", "Callisto"]

input :: [Moon]
input =
  mkMoons [V3 9 13 (-8), V3 (-3) 16 (-17), V3 (-4) 11 (-10), V3 0 (-2) (-2)]

test :: [Moon]
test = mkMoons [V3 (-1) 0 2, V3 2 (-10) (-7), V3 4 (-8) 8, V3 3 5 (-1)]

test2 :: [Moon]
test2 = mkMoons [V3 (-8) (-10) 0, V3 5 5 10, V3 2 (-7) 3, V3 9 (-8) (-3)]

pairsOf :: [Moon] -> [(Moon, [Moon])]
pairsOf xs = map (\x -> (x, filter ((/=) x) xs)) xs

gravity :: V3 Integer -> [V3 Integer] -> V3 Integer
gravity x = getSum . foldMap (Sum . liftU2 velChange x)
 where
  velChange a b = case compare a b of
    EQ -> 0
    GT -> -1
    LT -> 1

data Moon = Moon
    { velocity :: V3 Integer
    , name :: Text
    , pos :: V3 Integer
    }
    deriving (Eq, Show)

moonStep :: (Moon, [Moon]) -> Moon
moonStep (m@(Moon { pos = p, velocity }), vs) =
  let velocity' = velocity + gravity p (map pos vs)
  in m { velocity = velocity', pos = p + velocity' }

simStep :: [Moon] -> [Moon]
simStep = map moonStep . pairsOf

energy :: Moon -> Sum Integer
energy Moon {..} =
  (*) <$> foldMap (Sum . abs) velocity <*> foldMap (Sum . abs) pos

runSim :: Int -> [Moon] -> Sum Integer
runSim n = foldMap energy . Unsafe.head . drop n . iterate simStep

part1 :: Sum Integer
part1 = runSim 1000 input

sumVelocityOf :: Lens' (V3 Integer) Integer -> [Moon] -> Sum Integer
sumVelocityOf l = foldMap (Sum . abs . view l . velocity)

repeatsOn :: [Moon] -> Integer
repeatsOn ms = 2 * firstOccurance
 where
  firstOccurance = lcm (lcm x y) (lcm y z)
  x = zeroAxis _x
  y = zeroAxis _y
  z = zeroAxis _z
  zeroAxis :: Lens' (V3 Integer) Integer -> Integer
  zeroAxis l =
    fst $ Unsafe.head $ dropWhile ((/= 0) . sumVelocityOf l . snd) steps
  steps = drop 1 $ zip [0 ..] $ iterate simStep ms

part2 :: Integer
part2 = repeatsOn input
