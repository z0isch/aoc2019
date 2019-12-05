module Day5 where

import Relude hiding (Op)

import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

data ParamMode = Pos | Imm
    deriving (Eq, Show)

data Param = Param
    { mode :: !ParamMode
    , val :: !Integer
    }
    deriving (Eq, Show)

data Op
    = Add !Param !Param !Param
    | Multiply !Param !Param !Param
    | Input !Integer
    | Output !Integer
    deriving (Eq, Show)

type Parser = Parsec Void Text

opP :: Parser Op
opP = try (binOpP Add 1) <|> try (binOpP Multiply 2) <|> inputP <|> outputP

commaOrEOF :: Parser ()
commaOrEOF = void (char ',') <|> eof

paramP :: Parser Integer
paramP = signed (pure ()) decimal <* commaOrEOF

inputP :: Parser Op
inputP = Input <$> (char '3' *> char ',' *> paramP)

outputP :: Parser Op
outputP = Output <$> (char '4' *> char ',' *> paramP)

binOpP :: (Param -> Param -> Param -> Op) -> Integer -> Parser Op
binOpP op code = reverse <$> opCodeP >>= \case
  (a1 : a2 : pms) -> do
    (pm1 : pm2 : mpm3) <- forM pms $ \case
      '0' -> pure Pos
      '1' -> pure Imm
      p -> fail $ "ParamMode cannot be: " <> show p
    let pm3 = fromMaybe Pos $ fmap head $ nonEmpty mpm3
    case readMaybe [a2, a1] of
      Just c | c == code -> do
        (v1 : v2 : v3 : []) <- count 3 paramP
        pure $ op (Param pm1 v1) (Param pm2 v2) (Param pm3 v3)
      _ -> fail $ "Unknown OpCode: " <> [a2, a1]
  a -> fail $ "OpCode for binary op must be 4 or 5 digits not: " <> show
    (reverse a)
 where
  opCodeP :: Parser [Char]
  opCodeP = digitChar `manyTill` char ','

test :: Text
test = "1002,4,3,4"
