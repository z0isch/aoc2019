module Day5 where

import Relude hiding (Op)

import Data.Maybe (fromJust)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Vector (Vector, (!?), (!), (//))
import qualified Data.Vector as V
import Data.Text (pack)
import Control.Monad.Loops

data ParamMode = Pos | Imm
    deriving (Eq, Show)

data Param = Param
    { mode :: !ParamMode
    , val :: !Int
    }
    deriving (Eq, Show)

data Op
    = Add !Param !Param !Int
    | Multiply !Param !Param !Int
    | Input !Int
    | Output !Int
    | Halt
    | JIT !Param !Param
    | JIF !Param !Param
    | LTC !Param !Param !Int
    | EQC !Param !Param !Int
    deriving (Eq, Show)

type Parser = Parsec Void Text

opP :: Parser Op
opP = do
  o <- paramP
  (c, pm1, pm2, _) <- getOpCode o
  case c of
    1 -> do
      (v1 : v2 : v3 : []) <- count 3 paramP
      pure $ Add (Param pm1 v1) (Param pm2 v2) v3
    2 -> do
      (v1 : v2 : v3 : []) <- count 3 paramP
      pure $ Multiply (Param pm1 v1) (Param pm2 v2) v3
    3 -> Input <$> paramP
    4 -> Output <$> paramP
    5 -> do
      (v1 : v2 : []) <- count 2 paramP
      pure $ JIT (Param pm1 v1) (Param pm2 v2)
    6 -> do
      (v1 : v2 : []) <- count 2 paramP
      pure $ JIF (Param pm1 v1) (Param pm2 v2)
    7 -> do
      (v1 : v2 : v3 : []) <- count 3 paramP
      pure $ LTC (Param pm1 v1) (Param pm2 v2) v3
    8 -> do
      (v1 : v2 : v3 : []) <- count 3 paramP
      pure $ EQC (Param pm1 v1) (Param pm2 v2) v3
    99 -> pure Halt
    _ -> fail $ "Unkown opcode: " <> show o
 where
  paramP = signed (pure ()) decimal <* (void (char ',') <|> eof)
  getOpCode x =
    (,,,)
      <$> pure (x `mod` 100)
      <*> mkParamMode ((x `div` 100) `mod` 10)
      <*> mkParamMode ((x `div` 1000) `mod` 10)
      <*> mkParamMode ((x `div` 10000) `mod` 10)
  mkParamMode 0 = pure Pos
  mkParamMode 1 = pure Imm
  mkParamMode p = fail $ "ParamMode cannot be: " <> show p

data ProgStatus = Ready | AwaitingInput | Halted
  deriving (Eq, Show)

data Prog = Prog
  { status :: ProgStatus
  , input :: ![Int]
  , output :: ![Int]
  , instructionPointer :: !Int
  , instructions :: !(Vector Int)
  }
  deriving (Eq, Show)

data ProgError = ParserError (ParseErrorBundle Text Void) | BadPointer Int | NoMoreInput | InstructionAfterHalted
  deriving (Eq, Show)

getVal :: Vector Int -> Param -> Either ProgError Int
getVal vs Param {..} = case mode of
  Imm -> Right val
  Pos -> maybe (Left (BadPointer val)) Right $ vs !? val

step :: Prog -> (Either ProgError Prog)
step p@(Prog {..}) = case status of
  Halted -> Left InstructionAfterHalted
  _ -> case instructions !? instructionPointer of
    Nothing -> Left $ BadPointer instructionPointer
    Just _ ->
      case
          runParser
            opP
            ""
            (pack $ concat $ intersperse "," $ toList $ fmap show $ V.drop
              instructionPointer
              instructions
            )
        of
          Left e -> Left $ ParserError e
          Right op -> case op of

            Halt -> Right $ p { status = Halted }

            Input pos -> case head <$> (nonEmpty input) of
              Nothing
                | status == AwaitingInput -> Left NoMoreInput
                | otherwise -> Right $ p { status = AwaitingInput }
              Just i -> Right $ p
                { status = Ready
                , instructions = instructions // [(pos, i)]
                , instructionPointer = instructionPointer + 2
                , input = maybe [] tail $ nonEmpty input
                }

            Output pos -> Right $ p
              { output = instructions ! pos : output
              , instructionPointer = instructionPointer + 2
              }

            Add p1 p2 o -> do
              v <- (+) <$> getVal instructions p1 <*> getVal instructions p2
              pure $ p
                { instructions = instructions // [(o, v)]
                , instructionPointer = instructionPointer + 4
                }
            Multiply p1 p2 o -> do
              v <- (*) <$> getVal instructions p1 <*> getVal instructions p2
              pure $ p
                { instructions = instructions // [(o, v)]
                , instructionPointer = instructionPointer + 4
                }

            JIT p1 p2 -> do
              (v1, v2) <-
                (,) <$> getVal instructions p1 <*> getVal instructions p2
              pure $ p
                { instructionPointer = if v1 /= 0
                  then v2
                  else instructionPointer + 3
                }

            JIF p1 p2 -> do
              (v1, v2) <-
                (,) <$> getVal instructions p1 <*> getVal instructions p2
              pure $ p
                { instructionPointer = if v1 == 0
                  then v2
                  else instructionPointer + 3
                }

            LTC p1 p2 o -> do
              (v1, v2) <-
                (,) <$> getVal instructions p1 <*> getVal instructions p2
              pure $ p
                { instructions = instructions // [(o, if v1 < v2 then 1 else 0)]
                , instructionPointer = instructionPointer + 4
                }

            EQC p1 p2 o -> do
              (v1, v2) <-
                (,) <$> getVal instructions p1 <*> getVal instructions p2
              pure $ p
                { instructions =
                  instructions // [(o, if v1 == v2 then 1 else 0)]
                , instructionPointer = instructionPointer + 4
                }


runProg :: Prog -> (Either ProgError Prog)
runProg = iterateUntilM ((== Halted) . status) step

dayInput :: String -> IO (Vector Int)
dayInput fn =
  V.fromList . map (fromJust . readMaybe) . sepBy' ',' <$> readFile fn

sepBy' :: Char -> String -> [String]
sepBy' _ [] = []
sepBy' c s = cons $ case break (== c) s of
  (l, s') ->
    ( l
    , case s' of
      [] -> []
      _ : s'' -> sepBy' c s''
    )
  where cons ~(h, t) = h : t

part1 :: IO (Either ProgError Prog)
part1 = runProg . Prog Ready [1] [] 0 <$> dayInput "data/day5.txt"

part2 :: IO (Either ProgError Prog)
part2 = runProg . Prog Ready [5] [] 0 <$> dayInput "data/day5.txt"
