--day7.hs
--Preston Engler

module Day7 where

import Debug.Trace
import Data.Char (isDigit)
import Data.Int (Int64)

type Operators = Int64 -> Int64 -> Int64

op1 = [(+), (*)]
op2 = [(+), (*), (\a b -> read (show b ++ show a))]

day7 :: [String] -> String 
day7 s = show $ sum $ map (solve op1 . parse) s

day7p2 :: [String] -> String 
day7p2 s = show $ sum $ map (solve op2 . parse) s

parse :: String -> (Int64, [Int64])
parse s = (target, numbers)
  where 
    target = read $ takeWhile isDigit $ head $ words s
    numbers = reverse $ map read (tail $ words s)

--Returns the target number if it can be made through the use of the operators, otherwise returns 0
solve :: [Operators]-> (Int64, [Int64]) -> Int64
--solve operators (target, numbers) | trace (show $ possible operators numbers) False = undefined
solve operators (target, numbers) = if target `elem` possible operators numbers then target else 0

possible :: [Operators] -> [Int64] -> [Int64]
possible _ [number] = [number]
possible operators numbers = operators >>= f
  where 
   f :: Operators -> [Int64]
   f op = map (op $ head numbers) (possible operators $ tail numbers)
