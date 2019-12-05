module Day3 where

import Relude hiding (tail)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.List (minimum)
import qualified Data.Sequence as S
import Data.Sequence (Seq(..), (><))
import Linear.V2

import Numeric.Interval

data Segment = Segment {dir :: LineDir, start :: !(V2 Integer), end :: !(V2 Integer)}
  deriving (Eq, Show)

data LineDir = Vert | Horiz
  deriving (Eq, Show)

data Dir = L | R | U | D
    deriving (Eq, Show)

type Path = [(Dir, Integer)]

type Parser = Parsec Void Text

pathP :: Parser Path
pathP = p `sepBy` char ','
 where
  p = (,) <$> dirP <*> decimal
  dirP = asum [L <$ char 'L', R <$ char 'R', D <$ char 'D', U <$ char 'U']

inputP :: Parser (Path, Path)
inputP = (,) <$> pathP <*> (newline *> pathP)

input :: IO (Maybe (Path, Path))
input = parseMaybe inputP <$> readFileText "data/day3.txt"

runPath :: Path -> Seq Segment
runPath = foldl' (flip goPath) mempty
 where
  lineDir (U, _) = Vert
  lineDir (D, _) = Vert
  lineDir (L, _) = Horiz
  lineDir (R, _) = Horiz
  goDir (d, s) (V2 x y) = case d of
    D -> V2 x (y - s)
    U -> V2 x (y + s)
    L -> V2 (x - s) y
    R -> V2 (x + s) y
  goPath d =
    let ld = lineDir d
    in
      \case
        Empty -> pure $ Segment ld (V2 0 0) (goDir d (V2 0 0))
        ps@(_ :|> (Segment { end })) -> ps :|> Segment ld end (goDir d end)

manhatten :: V2 Integer -> V2 Integer -> Integer
manhatten (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

mkInterval :: Ord a => a -> a -> Interval a
mkInterval x y = if y >= x then x ... y else y ... x

segmentIntersect :: Segment -> Segment -> Maybe (V2 Integer)
segmentIntersect (Segment _ (V2 hx1 hy1) (V2 hx2 _)) (Segment _ (V2 vx1 vy1) (V2 _ vy2))
  | hy1 `member` vys && vx1 `member` hxs
  = Just (V2 vx1 hy1)
  | otherwise
  = Nothing
 where
  hxs = mkInterval hx1 hx2
  vys = mkInterval vy1 vy2

intersections :: Seq Segment -> Seq Segment -> [V2 Integer]
intersections s1 s2 =
  filter (/= V2 0 0)
    $ catMaybes
    $ toList
    $ (segmentIntersect <$> hs2 <*> vs1)
    >< (segmentIntersect <$> hs1 <*> vs2)
 where
  horiz = S.partition ((== Horiz) . dir)
  (hs1, vs1) = horiz s1
  (hs2, vs2) = horiz s2

part1 :: IO (Maybe Integer)
part1 =
  fmap
      (minimum
      . map (manhatten (V2 0 0))
      . uncurry intersections
      . bimap runPath runPath
      )
    <$> input

pointIntersect :: V2 Integer -> Segment -> Bool
pointIntersect (V2 x y) (Segment Horiz (V2 sx sy) (V2 endx _)) =
  y == sy && x `member` mkInterval sx endx
pointIntersect (V2 x y) (Segment Vert (V2 sx sy) (V2 _ endy)) =
  x == sx && y `member` mkInterval sy endy

distToIntersection :: V2 Integer -> Seq Segment -> Integer
distToIntersection i p = beforeSum + afterSum
 where
  beforeSum =
    sum $ fmap (\(Segment { start, end }) -> manhatten start end) before
  afterSum = maybe 0 (manhatten i . start) $ S.lookup 0 after
  (before, after) = S.breakl (pointIntersect i) p

part2 :: IO Integer
part2 = do
  Just (p1, p2) <- fmap (bimap runPath runPath) <$> input
  let
    is = intersections p1 p2
      <&> \i -> distToIntersection i p1 + distToIntersection i p2
  pure $ minimum is
