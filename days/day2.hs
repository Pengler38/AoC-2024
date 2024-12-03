--day2.hs
--Preston Engler

module Day2 (day2, day2p2) where 

data Trajectory = Increasing | Decreasing

parse :: [String] -> [[Int]]
parse s = (map . map) (read :: String -> Int) (map words s)

day2 :: [String] -> String
day2 s = show $ solve $ parse s

solve :: [[Int]] -> Int
solve xss = length $ filter isSafeNoExceptions xss
  where 
    isSafeNoExceptions xs = isSafe Increasing 0 xs || isSafe Decreasing 0 xs

isSafe :: Trajectory -> Int -> [Int] -> Bool
isSafe _ exceptions _ | exceptions < 0 = False
isSafe _ _ [_] = True
isSafe t exceptions (a:b:xs) = let 
  safe = (abs (a - b) <= 3 && a /= b) 
  sameTrajectory = case t of
    Increasing -> a < b 
    Decreasing -> a > b
  in if not safe || not sameTrajectory then isSafe t (exceptions-1) (a:xs)
    else isSafe t (exceptions-1) (a:xs) || isSafe t exceptions (b:xs)

day2p2 :: [String] -> String
day2p2 s = show $ solvep2 $ parse s

solvep2 :: [[Int]] -> Int
solvep2 xss = length $ filter isSafeSomeExceptions xss
  where 
    isSafeSomeExceptions xs = isSafe Increasing 1 xs || isSafe Decreasing 1 xs 
      || isSafe Increasing 0 (tail xs) || isSafe Decreasing 0 (tail xs) --Take care of the edge case where the exception is the first element
