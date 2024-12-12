--day8.hs
--Preston Engler

module Day8 where 

import Debug.Trace
import Data.List
import Data.Char (isLetter, isDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

--instance Ord (Int, Int) where

type Antennas = Map Char [(Int, Int)]
type Antinodes = Set.Set (Int, Int)

day8 :: [String] -> String 
day8 s = (show . Set.size . Set.filter (inBoundsArr s) . getAntinodes' . getAntennas) s

getAntennas :: [String] -> (Antennas, (Int, Int)) 
getAntennas s = let sPos = [(e, [(x, y)]) | (x, row) <- zip [0..] s, (y, e) <- zip [0..] row] :: [(Char, [(Int, Int)])]
  in (Map.fromListWith (++) $ filter ((\e -> isLetter e || isDigit e) . fst) sPos,  (length (head s), length s))

getAntinodes :: (Antennas, (Int, Int)) -> Antinodes 
getAntinodes (a, bounds) = (Set.fromList . concat . Map.elems . fmap (concat . map (antennasToAntinodesBounded bounds) . combinations 2)) a

--Only gets the first 2 antinodes
getAntinodes' :: (Antennas, (Int, Int)) -> Antinodes 
getAntinodes' (a, bounds) = (Set.fromList . concat . Map.elems . fmap (concat . map (antennasToAntinodes) . combinations 2)) a

--Gets the combinations of length i
combinations :: Int -> [a] -> [[a]]
combinations i xs = filter (\e -> length e == i) $ subsequences xs

--Errors if given a list where length /= 2
--String is passed in for bounds checking
antennasToAntinodesBounded :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
antennasToAntinodesBounded bounds [(x1, y1),(x2, y2)] = left ++ right
  where 
    (xDiff, yDiff) = (x1-x2, y1-y2)
    left = takeWhile (inBounds bounds) [(x1 + m*xDiff, y1 + m*yDiff) | m <- [0..]]
    right = takeWhile (inBounds bounds) [(x2 - m*xDiff, y2 - m*yDiff) | m <- [0..]]

--Gets only the first 2 antinodes
antennasToAntinodes :: [(Int, Int)] -> [(Int, Int)]
antennasToAntinodes [(x1, y1),(x2, y2)] = let (xDiff, yDiff) = (x1-x2, y1-y2)
  in (x1 + xDiff, y1 + yDiff) : (x2 - xDiff, y2 - yDiff) : []

inBounds :: (Int, Int) -> (Int, Int) -> Bool 
inBounds (maxX, maxY) (x, y) = x >= 0 && y >= 0 && x < maxX && y < maxY

inBoundsArr :: [[a]] -> (Int, Int) -> Bool 
inBoundsArr arr = inBounds (length (head arr), length arr)

day8p2 :: [String] -> String 
day8p2 s = (show . Set.size . getAntinodes . getAntennas) s
