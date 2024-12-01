--main.hs
--Preston Engler

import System.IO
import Data.List (sort)

main = do
  input <- getInput 1
  putStrLn $ show $ day1 input

day1 :: String -> Int
day1 s = sumDiff (tupleSort $ parse (lines s))
  where 
  tupleSort (xs, ys) = (sort xs, sort ys)
  sumDiff :: ([Int], [Int]) -> Int
  sumDiff (a, b) = foldl (\acc (a, b) -> acc + (abs $ a - b) ) 0 $ zip a b
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

getInput :: Int -> IO String
getInput i = readFile ("day/" ++ show i ++ "/input.txt")

