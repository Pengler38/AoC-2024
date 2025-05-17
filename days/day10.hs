module Day10 (day10, day10p2) where

-- Finally learned ST, experimenting with ReaderT a bit.
-- Might be able to make it cleaner by using ReaderT Identity (?) Not sure.

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Array as Array
import qualified Control.Monad.ST as ST
import qualified Data.Array.ST as STArray
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Class as T

data Dir = North | East | South | West
  deriving (Eq)

allDirs :: [Dir]
allDirs = [North, East, South, West]

type Pos = (Int, Int)

nextPos :: Pos -> Dir -> Pos
nextPos (x, y) dir = case dir of
  North -> (x,   y-1)
  East  -> (x+1, y  )
  South -> (x,   y+1)
  West  -> (x-1, y  )

prevDir dir = case dir of
  North -> South
  East  -> West
  South -> North
  West  -> East

inBounds :: (Pos, Pos) -> Pos -> Bool
inBounds ((minX, minY), (maxX, maxY)) (x, y) =
  x >= minX && y >= minY && x <= maxX && y <= maxY

traceTrailST :: Int -> Pos -> STArray.STArray s Pos (Maybe Int) -> Reader.ReaderT Bool (ST.ST s) Int
traceTrailST prevHeight pos ref = do
  bounds <- T.lift $ STArray.getBounds ref
  if not $ inBounds bounds pos
    then return 0
    else do
      maybeHeight <- T.lift $ STArray.readArray ref pos
      case maybeHeight of
        Just height | height == prevHeight + 1 ->
          let
            -- Build a list of the unique arguments for the recursive traceTrail calls
            nextPositions :: [Pos]
            nextPositions = map (nextPos pos) allDirs
            -- results :: [ST.ST s Int]
            results = [traceTrailST height p ref | p <- nextPositions]
          in do
            eraseTrail <- Reader.ask
            Monad.when eraseTrail $ T.lift $ STArray.writeArray ref pos Nothing
            if height == 9
              then return 1
              else do
                n <- sequence results
                return (sum n)
        _ ->
          return 0

traceTrail :: Bool -> Int -> Pos -> Array.Array Pos (Maybe Int) -> Int
traceTrail eraseTrail prevHeight pos arr =
  ST.runST $ do
    ref <- STArray.newListArray (Array.bounds arr) (Array.elems arr)
    Reader.runReaderT (traceTrailST prevHeight pos ref) eraseTrail

parseChar :: Char -> Maybe Int
parseChar c = if Char.isDigit c
  then Just $ Char.digitToInt c
  else Nothing

parse :: [String] -> Array.Array Pos (Maybe Int)
parse s =
  let
    list :: [[Maybe Int]]
    list = map (map parseChar) s
    width = length $ head list
    height = length list
  in
  Array.array
    ((0, 0), (width-1, height-1))
    [((x, y), list List.!! y List.!! x) | x <- [0..width-1], y <- [0..height-1]]

solve :: Bool -> [String] -> String
solve eraseTrail s =
  let
    arr = parse s
    check :: (Pos, Maybe Int) -> Int
    check (pos, e) = if e == Just 0
      then traceTrail eraseTrail (-1) pos arr
      else 0
  in
  show $ List.foldl' (\i el -> i + check el) 0 (Array.assocs arr)

day10 :: [String] -> String
day10 = solve True

day10p2 :: [String] -> String
day10p2 = solve False
