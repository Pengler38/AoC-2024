module Day12 (day12, day12p2) where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Array as Array
import qualified Control.Monad.Reader as Reader


import qualified Control.Monad.ST as ST
import qualified Data.Array.ST as STArray
import qualified Data.Array.MArray as MArray

import Debug.Trace

data Dir = North | East | South | West
  deriving (Eq)

data CheckedChar = Checked Char | Unchecked Char
unwrap :: CheckedChar -> Char
unwrap cc = case cc of
  Checked c -> c
  Unchecked c -> c

allDirs :: [Dir]
allDirs = [North, East, South, West]

type Pos = (Int, Int)

nextPos :: Pos -> Dir -> Pos
nextPos (x, y) dir = case dir of
  North -> (x,   y-1)
  East  -> (x+1, y  )
  South -> (x,   y+1)
  West  -> (x-1, y  )

getNextPositions :: Pos -> [Pos]
getNextPositions p = map (nextPos p) allDirs

addTuple :: Pos -> Pos -> Pos
addTuple (a1, b1) (a2, b2) = (a1+a2, b1+b2)

inBounds :: (Pos, Pos) -> Pos -> Bool
inBounds ((minX, minY), (maxX, maxY)) (x, y) =
  x >= minX && y >= minY && x <= maxX && y <= maxY

safeRead :: STArray.STArray s Pos e -> Pos -> ST.ST s (Maybe e)
safeRead arr pos = do
  bounds <- STArray.getBounds arr
  if inBounds bounds pos then do
    res <- STArray.readArray arr pos
    return $ Just res
  else
    return Nothing

-- Returns (Area, Perimeter)
findAreaAndPerimeter :: Char -> Pos -> STArray.STArray s Pos CheckedChar -> ST.ST s (Int, Int)
findAreaAndPerimeter targetChar pos arr = do
  currentChar <- safeRead arr pos
  case currentChar of
    Just (Unchecked c) | c == targetChar -> do
      let nextPositions = getNextPositions pos
      STArray.writeArray arr pos (Checked c)
      neighboringChars <- sequence [safeRead arr pos' | pos' <- nextPositions]
      let
        area = 1
        perimeter = foldl (\acc cc -> if (unwrap <$> cc) /= Just targetChar then acc+1 else acc) 0 neighboringChars
        thisResult = (area, perimeter)
      results <- sequence [findAreaAndPerimeter targetChar pos' arr | pos' <- nextPositions]
      return $ List.foldl' addTuple thisResult results
    _ -> return (0, 0)

solve :: [String] -> Int
solve s =
  let height = length s; width = length $ head s in
  ST.runST $ do
    let bounds = ((0, 0), (width-1, height-1))
    arrRef <- STArray.newListArray bounds [Unchecked (s List.!! y List.!! x) | y <- [0..height-1], x <- [0..width-1]]
    let
      start pos = do
        cChar <- STArray.readArray arrRef pos
        case cChar of
          Unchecked c -> findAreaAndPerimeter c pos arrRef
          Checked c -> return (0, 0)
    assocs <- STArray.getAssocs arrRef
    let arrayIndices = map fst assocs
    Monad.foldM (\acc pos -> start pos >>= \(area, perim) -> return $ acc + (area*perim)) 0 arrayIndices

day12 :: [String] -> String
day12 = show . solve

day12p2 :: [String] -> String
day12p2 = undefined
