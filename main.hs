--main.hs
--Preston Engler

import System.IO
import System.Environment
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12

data Test = RunTest | NoTest

main = run 9

run :: Int -> IO ()
run i = run' i NoTest

runTest :: Int -> IO ()
runTest i = run' i RunTest

run' i test = do
  --args <- getArgs
  input <- mInput
  putStrLn $ day $ lines input
  putStrLn $ dayp2 $ lines input
  where 
    mInput = case test of 
      NoTest -> getInput i "/input.txt"
      RunTest -> getInput i "/test.txt"
    (day, dayp2) = case i of
      1 -> (day1, day1p2)
      2 -> (day2, day2p2)
      3 -> (day3, day3p2)
      4 -> (day4, day4p2)
      5 -> (day5, day5p2)
      6 -> (day6, day6p2)
      7 -> (day7, day7p2)
      8 -> (day8, day8p2)
      9 -> (day9, day9p2)
      10 -> (day10, day10p2)
      11 -> (day11, day11p2)
      12 -> (day12, day12p2)


getInput :: Int -> String -> IO String
getInput i s = readFile ("days/" ++ show i ++ s)
