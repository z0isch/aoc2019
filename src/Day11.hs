module Day11 where

import Relude

import Linear
import Day5 (Prog(..), ProgError(..), ProgStatus(..), step, dayInput)
import Control.Monad.Loops (iterateUntilM)
import qualified Data.Map.Strict as M
import Codec.Picture (generateImage, Image(..), PixelRGB8(..), writeBitmap)
import Data.List (minimumBy, maximumBy)

iterateUntiAwaitingInputOrHalt :: Prog -> (Either ProgError Prog)
iterateUntiAwaitingInputOrHalt p =
  step p
    >>= iterateUntilM ((\s -> s == Halted || s == AwaitingInput) . status) step

data Color = Black | White
    deriving (Eq, Show)

colorInt :: Color -> Integer
colorInt Black = 0
colorInt White = 1

intToColor :: Integer -> Maybe Color
intToColor 0 = Just Black
intToColor 1 = Just White
intToColor _ = Nothing

intToDir :: Integer -> Maybe (M22 Int)
intToDir 0 = Just $ V2 (V2 0 1) (V2 (-1) 0)
intToDir 1 = Just $ V2 (V2 0 (-1)) (V2 1 0)
intToDir _ = Nothing

data Robot = Robot
    { prog :: Prog
    , location :: V2 Int
    , orientation :: V2 Int
    , hull :: Map (V2 Int) Color
    }
    deriving (Eq, Show)

initialRobot :: Map (V2 Int) Color -> Prog -> Robot
initialRobot hull prog =
  Robot { prog, location = V2 0 0, orientation = V2 0 1, hull }

runRobotStep :: Robot -> Either ProgError Robot
runRobotStep r@(Robot {..}) = do
  let
    prog' = prog
      { input = (colorInt (fromMaybe Black (M.lookup location hull)))
        : input prog
      }
  prog''@Prog { output = output' } <- iterateUntiAwaitingInputOrHalt prog'
  (dirInt, cInt) <-
    maybeToRight NotEnoughOutput
    $ (\(c :| os) -> (,) c . head <$> nonEmpty os)
    =<< nonEmpty output'
  c <- maybeToRight BadOutput $ intToColor cInt
  d <- maybeToRight BadOutput $ intToDir dirInt
  let
    hull' = M.insert location c hull
    orientation' = d !* orientation
    location' = location + orientation'
  pure $ r
    { prog = prog''
    , hull = hull'
    , location = location'
    , orientation = orientation'
    }

runRobot :: Robot -> Either ProgError Robot
runRobot = iterateUntilM ((== Halted) . status . prog) runRobotStep

part1 :: IO (Either ProgError Int)
part1 =
  fmap (M.size . hull)
    . runRobot
    . initialRobot mempty
    . Prog Ready [] [] 0 0
    <$> dayInput "./data/day11.txt"

mkImage :: (Map (V2 Int) Color) -> Int -> Int -> Image PixelRGB8
mkImage hull = generateImage
  (\x y -> pixelColor $ fromMaybe Black $ M.lookup (V2 x y) hull)
 where
  pixelColor White = PixelRGB8 255 255 255
  pixelColor Black = PixelRGB8 0 0 0

part2 :: IO ()
part2 = do
  er <-
    runRobot
    . initialRobot (M.fromList [(V2 0 0, White)])
    . Prog Ready [] [] 0 0
    <$> dayInput "./data/day11.txt"
  case er of
    Right r -> do
      let
        roatedHull = M.mapKeys ((V2 (V2 (-1) 0) (V2 0 (-1)) !*)) $ hull r
        V2 maxX _ =
          maximumBy (\(V2 x _) (V2 y _) -> compare x y) $ M.keys roatedHull
        V2 _ maxY =
          maximumBy (\(V2 _ x) (V2 _ y) -> compare x y) $ M.keys roatedHull
      writeBitmap "./output/day11-part2.bmp"
        $ mkImage roatedHull (maxX + 1) (maxY + 1)
    Left err -> print err


