--main.hs
--Preston Engler

import System.IO
import System.Environment
import Day1

data Test = RunTest | NoTest

run :: Int -> IO ()
run i = run' i NoTest

runTest :: Int -> IO ()
runTest i = run' i RunTest

run' i test = do
  --args <- getArgs
  input <- mInput
  putStrLn $ show $ day $ lines input
  putStrLn $ show $ dayp2 $ lines input
  where 
    mInput = case test of 
      NoTest -> getInput i "/input.txt"
      RunTest -> getInput i "/test.txt"
    (day, dayp2) = case i of
      1 -> (day1, day1p2)


getInput :: Int -> String -> IO String
getInput i s = readFile ("days/" ++ show i ++ s)
