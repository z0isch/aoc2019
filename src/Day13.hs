module Day13 where

import Relude

import Linear
import qualified Relude.Unsafe as Unsafe
import Day5 (Prog(..), ProgError(..), ProgStatus(..), step, runProg, dayInput)
import qualified Data.Map.Strict as M
import Control.Monad.Loops (iterateUntilM)
import Codec.Picture (generateImage, Image(..), PixelRGB8(..), writeBitmap)
import Data.List (maximum)
import qualified Data.Sequence as Seq

data Tile = Wall | Block | HPaddle | Ball
    deriving (Eq, Show)

type Tiles = Map (V2 Integer) (Maybe Tile)

type Score = Integer

data Move = Neutral | TL | TR
    deriving (Eq, Show)

moveInput :: Move -> Integer
moveInput Neutral = 0
moveInput TL = (-1)
moveInput TR = 1

parseTile 1 = Just Wall
parseTile 2 = Just Block
parseTile 3 = Just HPaddle
parseTile 4 = Just Ball
parseTile _ = Nothing

parseOutput :: [Integer] -> Maybe (Score, Tiles)
parseOutput = go (0, mempty) . reverse
 where
  go m [] = Just m
  go (_, m) ((-1) : 0 : score : xs) = go (score, m) xs
  go (s, m) (x : y : t : xs) = go (s, M.insert (V2 x y) (parseTile t) m) xs
  go _ _ = Nothing

numBlocks :: Prog -> Maybe Int
numBlocks =
  fmap (M.size . M.filter (== Just Block) . snd) . parseOutput . output

part1 :: IO (Either ProgError (Maybe Int))
part1 = fmap numBlocks . runProg . Prog Ready [] [] 0 0 <$> dayInput
  "./data/day13.txt"

insertQuarter :: Prog -> Prog
insertQuarter p@Prog {..} = p { instructions = M.insert 0 2 instructions }

gameStep :: Prog -> (Either ProgError Prog)
gameStep p =
  step p
    >>= iterateUntilM ((\s -> s == Halted || s == AwaitingInput) . status) step

chooseMoves :: GameState -> [Move]
chooseMoves GameState { prog = Prog {..}, board } = filter
  (isInRange . mkPaddleMove paddleX)
  [TL, TR, Neutral]
 where
  ((V2 ballX _), _) = M.findMin $ M.filter (== Just Ball) board
  ((V2 paddleX _), _) = M.findMin $ M.filter (== Just HPaddle) board
  mkPaddleMove xCurr = \case
    Neutral -> xCurr
    TL -> xCurr - 1
    TR -> xCurr + 1
  isInRange paddleX' = abs (ballX - paddleX') < 2

inputMove :: Prog -> Move -> Prog
inputMove p@Prog {..} m = p { input = moveInput m : input }

part2 = do
  eg <- gameStep . insertQuarter . Prog Ready [] [] 0 0 <$> dayInput
    "./data/day13.txt"
  case eg of
    Left e -> error $ show e
    Right g -> do
      let
        (s, b) = Unsafe.fromJust $ parseOutput $ output g
        gs = GameState b s (g { output = [] })
      gs' <- gameLoop 0 [(gs, chooseMoves gs)]
      pure $ score gs'

mkImage :: GameState -> Image PixelRGB8
mkImage GameState { prog = Prog {..}, board } = generateImage
  (\x y -> maybe (PixelRGB8 0 0 0) pixelColor
    $ M.lookup (V2 (fromIntegral x) (fromIntegral y)) board
  )
  height
  width
 where
  height =
    (+ 1) $ maximum $ fmap (fromIntegral . (\(V2 y _) -> y)) $ M.keys board
  width =
    (+ 1) $ maximum $ fmap (fromIntegral . (\(V2 _ x) -> x)) $ M.keys board
  pixelColor (Just Wall) = PixelRGB8 255 255 255
  pixelColor (Just Block) = PixelRGB8 0 255 0
  pixelColor (Just HPaddle) = PixelRGB8 0 0 255
  pixelColor (Just Ball) = PixelRGB8 255 0 0
  pixelColor Nothing = PixelRGB8 0 0 0

data GameState = GameState
    { board :: Tiles
    , score :: Integer
    , prog :: Prog
    }
    deriving (Eq, Show)

updateGameState :: GameState -> Prog -> GameState
updateGameState g@GameState {..} p@Prog {..} =
  let (score', board') = Unsafe.fromJust $ parseOutput output
  in
    g
      { prog = p { output = [] }
      , score = if score' /= 0 then score' else score
      , board = M.union board' board
      }

gameLoop :: Integer -> [(GameState, [Move])] -> IO GameState
gameLoop _ [] = error "Out of options"
gameLoop n ((_, []) : oldStates) = gameLoop (n - 1) oldStates
gameLoop n ((gs@(GameState { prog }), (m : ms)) : oldStates) =
  case gameStep (inputMove prog m) of
    Left e -> error $ show e
    Right prog' ->
      let
        currentStates = ((gs, ms) : oldStates)
        gs' = updateGameState gs prog'
        numBlocks = M.size $ M.filter (== Just Block) $ board gs'
      in if numBlocks == 0
        then do
          writeBitmap ("./output/day13/" <> show n <> ".bmp") $ mkImage gs'
          pure gs'
        else case status prog' of
          Halted -> case ms of
            [] -> do
              writeBitmap ("./output/day13/backtrack-" <> show n <> ".bmp")
                $ mkImage gs'
              gameLoop (n - 1) oldStates
            _ -> gameLoop n currentStates
          _ -> do
            when ((n `mod` 5 == 0) || n == 396)
              $ writeBitmap ("./output/day13/" <> show n <> ".bmp")
              $ mkImage gs'
            gameLoop (n + 1) ((gs', chooseMoves gs') : currentStates)

