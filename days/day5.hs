--day5.hs
--Preston Engler

module Day5 where 

import Data.Char (isDigit)
import Debug.Trace (trace)
import Data.List
--import Data.HashMap

day5 :: [String] -> String
day5 = show . (solve False) . parse


parse :: [String] -> ([(Int, Int)], [[Int]])
parse s = (
    map parseTop $ takeWhile (/= []) s,
    map parseBottom $ drop 1 $ dropWhile (/= []) s
  )
  where 
    parseTop e = (read $ takeWhile isDigit e, read $ drop 1 $ dropWhile (/= '|') e)
    parseBottom :: String -> [Int]
    parseBottom [] = []
    parseBottom e = (read $ takeWhile isDigit e :: Int) : (parseBottom $ drop 1 $ dropWhile (/= ',') e)


solve :: Bool -> ([(Int, Int)], [[Int]]) -> Int
solve fixNums (rules, numbersList) = sum $ map (applyRules fixNums True rules) numbersList


applyRules :: Bool -> Bool -> [(Int, Int)] -> [Int] -> Int
applyRules fixNums isFirstTry rules numbers = case fixNums of
    False -> if correct then middle numbers else 0
    True  -> 
      if correct then 
        if not isFirstTry then middle numbers else 0
      else applyRules fixNums False rules (fix numbers failedIndex)
  where 
    (correct, failedIndex) = checkRules rules numbers


checkRules [] _ = (True, -1)
checkRules rules numbers = if correct then checkRules (tail rules) numbers else (False, failedIndex)
  where (correct, failedIndex) = checkRule (head rules) numbers


checkRule rule numbers = case (elemIndex (fst rule) numbers, elemIndex (snd rule) numbers) of
  (Just x, Just y) -> if x < y then (True, -1) else (False, x)
  otherwise -> (True, -1)


middle :: [Int] -> Int
middle xs = xs !! (length xs `div` 2)


fix :: [Int] -> Int -> [Int]
fix xs i | i < 1 = undefined
fix xs i = if i == 1 then 
    xs !! 1 : head xs : drop 2 xs 
  else 
    head xs : fix (tail xs) (i-1)


sortTuple :: [(Int, Int)] -> [(Int, Int)]
sortTuple [] = []
sortTuple xs = sortTuple lesser ++ [head xs] ++ sortTuple greater
  where 
    lesser = filter (\e -> fst e < fst (head xs)) (tail xs)
    greater = filter (\e -> fst e >= fst (head xs)) (tail xs)


--In part 2, sort the Rules to prevent rules flipping the same 2 numbers in the fix function
--It's not the fastest running solution... But it works!
day5p2 :: [String] -> String
day5p2 s = show $ solve True (sortedRules, numbers)
  where 
    (rules, numbers) = parse s
    sortedRules = sortTuple rules
