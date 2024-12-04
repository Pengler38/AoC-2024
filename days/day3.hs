--day3.hs
--Preston Engler

module Day3(day3, day3p2) where

import Text.Read
import Data.Char
import Debug.Trace

day3 :: [String] -> String
day3 s = show $ foldl (\acc e -> mul e + acc) 0 s

mul :: String -> Int 
mul [] = 0
mul xs = let nextxs = drop 4 xs in
  if take 4 xs == "mul(" then foldl (*) 1 (numbers nextxs) + mul nextxs else mul (tail xs)

numbers :: String -> [Int]
numbers xs = if (take 1 $ dropWhile isDigit $ drop 1 $ dropWhile isDigit xs) == [')'] then
  [ forceReadMaybe $ takeWhile isDigit xs, forceReadMaybe $ takeWhile isDigit $ drop 1 $ dropWhile ((/=) ',') xs]
  else [0, 0]
  where 
    forceReadMaybe xs = case readMaybe xs of
      Just x -> x
      Nothing -> 0

data Enable = Disabled | Enabled

day3p2 :: [String] -> String
day3p2 s = show $ mul' True $ unlines s

mul' :: Bool -> String -> Int
mul' _ [] = 0
mul' enabled xs = let nextxs = drop 4 xs in
  if take 4 xs == "mul(" && enabled then foldl (*) 1 (numbers nextxs) + mul' enabled nextxs
  else if take 4 xs == "do()" then mul' True $ drop 4 xs
  else if take 7 xs == "don't()" then mul' False $ drop 7 xs
  else mul' enabled (tail xs)
