module Day5 where

import Relude hiding (Op)

import Data.Maybe (fromJust)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (pack)
import Control.Monad.Loops

data ParamMode = Pos | Imm | Rel
    deriving (Eq, Show)

data Param = Param
    { mode :: !ParamMode
    , val :: !Integer
    }
    deriving (Eq, Show)

data Op
    = Add !Param !Param !Param
    | Multiply !Param !Param !Param
    | Input !Param
    | Output !Param
    | Halt
    | JIT !Param !Param
    | JIF !Param !Param
    | LTC !Param !Param !Param
    | EQC !Param !Param !Param
    | AdjustRelativeBase !Param
    deriving (Eq, Show)

type Parser = Parsec Void Text

opP :: Parser Op
opP = do
  o <- paramP
  (c, pm1, pm2, pm3) <- getOpCode o
  case c of
    1 -> do
      (v1 : v2 : v3 : []) <- count 3 paramP
      pure $ Add (Param pm1 v1) (Param pm2 v2) (Param pm3 v3)
    2 -> do
      (v1 : v2 : v3 : []) <- count 3 paramP
      pure $ Multiply (Param pm1 v1) (Param pm2 v2) (Param pm3 v3)
    3 -> Input . Param pm1 <$> paramP
    4 -> Output . Param pm1 <$> paramP
    5 -> do
      (v1 : v2 : []) <- count 2 paramP
      pure $ JIT (Param pm1 v1) (Param pm2 v2)
    6 -> do
      (v1 : v2 : []) <- count 2 paramP
      pure $ JIF (Param pm1 v1) (Param pm2 v2)
    7 -> do
      (v1 : v2 : v3 : []) <- count 3 paramP
      pure $ LTC (Param pm1 v1) (Param pm2 v2) (Param pm3 v3)
    8 -> do
      (v1 : v2 : v3 : []) <- count 3 paramP
      pure $ EQC (Param pm1 v1) (Param pm2 v2) (Param pm3 v3)
    9 -> AdjustRelativeBase . Param pm1 <$> paramP
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
  mkParamMode 2 = pure Rel
  mkParamMode p = fail $ "ParamMode cannot be: " <> show p

data ProgStatus = Ready | AwaitingInput | Halted
  deriving (Eq, Show)

data Prog = Prog
  { status :: ProgStatus
  , input :: ![Integer]
  , output :: ![Integer]
  , instructionPointer :: !Integer
  , relativeBase :: Integer
  , instructions :: !(Map Integer Integer)
  }
  deriving (Eq, Show)

data ProgError = ParserError (ParseErrorBundle Text Void) | OutputToImm | BadPointer Integer | NoMoreInput | InstructionAfterHalted
  deriving (Eq, Show)

getIOPos :: Prog -> Param -> Either ProgError Integer
getIOPos Prog {..} Param {..} = case mode of
  Imm -> Left OutputToImm
  Pos -> Right val
  Rel -> Right $ val + relativeBase

getVal :: Prog -> Param -> Either ProgError Integer
getVal Prog {..} Param {..} = case mode of
  Imm -> Right val
  Pos -> Right $ fromMaybe 0 $ instructions M.!? val
  Rel -> Right $ fromMaybe 0 $ instructions M.!? (val + relativeBase)

step :: Prog -> (Either ProgError Prog)
step p@(Prog {..}) = case status of
  Halted -> Left InstructionAfterHalted
  _ -> case instructions M.!? instructionPointer of
    Nothing -> Left $ BadPointer instructionPointer
    Just _ ->
      case
          runParser
            opP
            ""
            (pack
            $ concat
            $ intersperse ","
            -- May need to fill in the spaces
            $ fmap (show . snd)
            $ genericDrop instructionPointer
            $ M.assocs instructions
            )
        of
          Left e -> Left $ ParserError e
          Right op -> case op of

            Halt -> Right $ p { status = Halted }

            Input i -> case head <$> (nonEmpty input) of
              Nothing
                | status == AwaitingInput -> Left NoMoreInput
                | otherwise -> Right $ p { status = AwaitingInput }
              Just v -> do
                pos <- getIOPos p i
                pure $ p
                  { status = Ready
                  , instructions = M.insert pos v instructions
                  , instructionPointer = instructionPointer + 2
                  , input = maybe [] tail $ nonEmpty input
                  }

            Output pos -> do
              o <- getVal p pos
              pure $ p
                { output = o : output
                , instructionPointer = instructionPointer + 2
                }

            Add p1 p2 o -> do
              v <- (+) <$> getVal p p1 <*> getVal p p2
              pos <- getIOPos p o
              pure $ p
                { instructions = M.insert pos v instructions
                , instructionPointer = instructionPointer + 4
                }

            Multiply p1 p2 o -> do
              v <- (*) <$> getVal p p1 <*> getVal p p2
              pos <- getIOPos p o
              pure $ p
                { instructions = M.insert pos v instructions
                , instructionPointer = instructionPointer + 4
                }

            JIT p1 p2 -> do
              (v1, v2) <- (,) <$> getVal p p1 <*> getVal p p2
              pure $ p
                { instructionPointer = if v1 /= 0
                  then v2
                  else instructionPointer + 3
                }

            JIF p1 p2 -> do
              (v1, v2) <- (,) <$> getVal p p1 <*> getVal p p2
              pure $ p
                { instructionPointer = if v1 == 0
                  then v2
                  else instructionPointer + 3
                }

            LTC p1 p2 o -> do
              (v1, v2) <- (,) <$> getVal p p1 <*> getVal p p2
              pos <- getIOPos p o
              pure $ p
                { instructions = M.insert
                  pos
                  (if v1 < v2 then 1 else 0)
                  instructions
                , instructionPointer = instructionPointer + 4
                }

            EQC p1 p2 o -> do
              (v1, v2) <- (,) <$> getVal p p1 <*> getVal p p2
              pos <- getIOPos p o
              pure $ p
                { instructions = M.insert
                  pos
                  (if v1 == v2 then 1 else 0)
                  instructions
                , instructionPointer = instructionPointer + 4
                }

            AdjustRelativeBase p1 -> do
              rel <- getVal p p1
              pure $ p
                { relativeBase = relativeBase + rel
                , instructionPointer = instructionPointer + 2
                }



runProg :: Prog -> (Either ProgError Prog)
runProg = iterateUntilM ((== Halted) . status) step

dayInput :: String -> IO (Map Integer Integer)
dayInput fn =
  M.fromList
    . zip [0 ..]
    . map (fromJust . readMaybe)
    . sepBy' ','
    <$> readFile fn

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
part1 = runProg . Prog Ready [1] [] 0 0 <$> dayInput "data/day5.txt"

part2 :: IO (Either ProgError Prog)
part2 = runProg . Prog Ready [5] [] 0 0 <$> dayInput "data/day5.txt"
