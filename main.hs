--main.hs
--Preston Engler

import System.IO
import Data.List

main = do
  input <- getInput 1
  putStrLn $ show $ day1 input
  putStrLn $ show $ day1p2 input

day1 :: String -> Int
day1 s = sumDiff (tupleSort $ parse (lines s))

tupleSort (xs, ys) = (sort xs, sort ys)

sumDiff :: ([Int], [Int]) -> Int
sumDiff (xs, ys) = foldl (\acc (x, y) -> acc + (abs $ x - y)) 0 $ zip xs ys

parse :: [String] -> ([Int], [Int])
parse s = parse' s ([], [])
  where
  parse' :: [String] -> ([Int], [Int]) -> ([Int], [Int])
  parse' [] (xs, ys) = (xs, ys)
  parse' s (xs, ys) = let 
    w = words $ head s
    x = read $ w !! 0
    y = read $ w !! 1 in 
    parse' (tail s) (x:xs, y:ys)

day1p2 :: String -> Int
day1p2 s = simScore (tupleSort $ parse (lines s)) 0
  where
  simScore :: ([Int], [Int]) -> Int -> Int
  simScore ([], ys) score = score
  simScore (x:xs, ys) score = simScore (xs, ys) (score + xScore)
    where xScore = x * (length $ elemIndices x ys)

getInput :: Int -> IO String
getInput i = readFile ("day/" ++ show i ++ "/input.txt")

