--day4.hs
--Preston Engler

module Day4 where

import Debug.Trace

data Direction = N | NE | E | SE | S | SW | W | NW 
allDirections = [N, NE, E, SE, S, SW, W, NW]
 
day4 :: [String] -> String
day4 s = show $ loop check 'X' s (zip indices $ concat s) 0
  where 
    indices = zip [e `mod` xLength s | e <- [0..]] [e `div` xLength s | e <- [0..]]

loop :: ([String] -> (Int, Int) -> Int) -> Char -> [String] -> [((Int, Int), Char)] -> Int -> Int 
loop func c s [] acc = acc
-- loop func c s tuples acc | trace ((show . snd . unzip) tuples ++ "  " ++ (show . fst . unzip)tuples ++ "  "++ show acc) False = undefined
loop func c s tuples acc = case nextX of
    [] -> acc
    _ -> loop func c s (tail nextX) (acc + (func s $ fst $ head nextX))
  where 
    nextX = dropWhile (\e -> snd e /= c) tuples

check :: [String] -> (Int, Int) -> Int 
check s index = foldr (\d acc -> if match "XMAS" index d s then acc + 1 else acc) 0 allDirections 

match :: String -> (Int, Int) -> Direction -> [String] -> Bool 
-- match target (x, y) dir s | trace (show x ++ "  " ++ show y) False = undefined
match [] _ _ _ = True 
match target (x, y) dir s =
  if get s (x, y) == head target then 
    let (newX, newY) = case dir of
          N  -> (x  , y-1)
          NE -> (x+1, y-1)
          E  -> (x+1, y  )
          SE -> (x+1, y+1)
          S  -> (x  , y+1)
          SW -> (x-1, y+1)
          W  -> (x-1, y  )
          NW -> (x-1, y-1)
    in match (tail target) (newX, newY) dir s 
  else False

get :: [String] -> (Int, Int) -> Char 
-- get s (x, y) | trace ( "  " ++ show x ++ "  " ++ show y ) False = undefined
get s (x, y) 
  | (x < 0 || x >= xLength s) || (y < 0 || y >= yLength s) = ' '
get s (x, y) = (s !! y) !! x

yLength :: [String] -> Int
yLength = length
xLength :: [String] -> Int
xLength = length . head

check' :: [String] -> (Int, Int) -> Int 
check' s (x, y) = if c (x+1, y+1) (x-1, y-1) && c (x-1, y+1) (x+1, y-1) then 1 else 0
  where c t1 t2 = case get s t1 of 
          'M' -> get s t2 == 'S'
          'S' -> get s t2 == 'M'
          otherwise -> False

day4p2 :: [String] -> String
day4p2 s = show $ loop check' 'A' s (zip indices $ concat s) 0
  where 
    indices = zip [e `mod` xLength s | e <- [0..]] [e `div` xLength s | e <- [0..]]
