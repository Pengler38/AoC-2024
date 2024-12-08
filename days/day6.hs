--day6.hs
--Preston Engler

--THIS IS EXTREMELY UNOPTIMIZED AND ALSO LONG BECAUSE I'M WRANGLING LISTS TO DO THINGS THEY REALLY SHOULDN'T BE DOING
--AT LEAST I THINK MAKING THE STATE DATA WAS COOL
--
--THE CODE SHALL BE LEFT THIS WAY INCLUDING THE DEBUG PRINTS AS A MONUMENT TO MY HUBRIS

module Day6 where 

import Debug.Trace
import Data.List

data Direction = UpD | RightD | LeftD | DownD deriving (Show, Eq)
data Finished = Looping | OutOfBounds
data State = State [String] (Int, Int) Direction deriving Show
--NOTE: State Equality does not check the String, only position and direction!
instance Eq State where 
  State sA (xA, yA) dA == State sB (xB, yB) dB = 
    xA == xB && yA == yB && dA == dB

stringFromState :: State -> [String]
stringFromState (State s _ _) = s

day6 :: [String] -> String 
day6 s | trace (unlines $ solvep1 0 $ State s (start s) UpD) False = undefined
day6 s = show $ sum $ map (length . filter (charIsDirection)) $ solvep1 0 $ State s (start s) UpD

solvep1 :: Int -> State  -> [String]
--solvep1 stateNum _ | trace ("    " ++ show stateNum) False = undefined
solvep1 stateNum currentState = case getNextState currentState of
    Right nextState -> 
        solvep1 (stateNum+1) nextState
    Left OutOfBounds -> stringFromState currentState

day6p2 :: [String] -> String 
day6p2 s = show $ length $ solvep2 startState 0 [] startState
  where startState = State s (start s) UpD

solvep2 :: State -> Int -> [(Int, Int)] -> State -> [(Int, Int)]
--solvep2 originalState stateNum accList state | if stateNum /= 4227 then False else trace (unlines $ stringFromState state) False = undefined
solvep2 originalState stateNum accList _ | trace ("    " ++ show stateNum ++ "    " ++ show (length accList)) False = undefined
solvep2 originalState stateNum accList currentState = case nextState of
  Left OutOfBounds -> accList
  Left Looping -> undefined
  Right s -> solvep2 originalState (stateNum+1) newAccList s
  where
    nextState = getNextState currentState
    (State str pos dir) = currentState
    (State origS origP origD) = originalState
    nextPosition = nextPos pos dir
    placedObjInFront = State (replace origS nextPosition '#') origP origD
    newAccList = if not (nextPosition `elem` accList) && makesLoop placedObjInFront then nextPosition:accList else accList

makesLoop :: State -> Bool
makesLoop state = case getNextState state of 
  Left Looping -> True
  Left OutOfBounds -> False 
  Right newState -> makesLoop newState

start :: [String] -> (Int, Int)
start s = (x, y)
  where 
    y = case findIndex (\e -> elemIndex '^' e /= Nothing) s of
      Just a -> a 
      Nothing -> undefined
    x = case elemIndex '^' (s !! y) of 
      Just a  -> a 
      Nothing -> undefined

getNextState :: State  -> Either Finished State
getNextState (State input (x, y) dir) 
  | isOutOfBounds input (x, y) = Left OutOfBounds
  | otherwise = 
    if looping then
      Left Looping
    else if not blocked then 
      Right (State output (newX, newY)    dir)
    else 
      Right (State output (   x,    y) newDir)
    where 
      nextChar = get input (newX, newY)
      currentChar = get input (x, y)
      blocked = nextChar == '#'
      newDir = nextDir dir
      (newX, newY) = nextPos (x, y) dir
      output = replace input (x, y) newChar
        --Use non-direction characters to indicate the spot is passed over multiple times. If it's passed through 4 times, then that means we're looping.
        where newChar = if currentChar == '+' then '?' else if currentChar == 'x' then '+' else if charIsDirection currentChar then 'x' else dirToChar dir
      looping = nextChar == '?'

nextPos :: (Int, Int) -> Direction -> (Int, Int)
nextPos (x, y) dir = case dir of 
  UpD    -> (x, y-1)
  RightD -> (x+1, y)
  DownD  -> (x, y+1)
  LeftD  -> (x-1, y)

nextDir dir = case dir of
  UpD    -> RightD
  RightD -> DownD
  DownD  -> LeftD
  LeftD  -> UpD

--                         |directions        |  2    3    4 passes
charIsDirection c = elem c ['^', '>', 'v', '<', 'x', '+', '?']

dirToChar dir = case dir of
  UpD    -> '^'
  RightD -> '>'
  DownD  -> 'v'
  LeftD  -> '<'

isOutOfBounds :: [String] -> (Int, Int) -> Bool
isOutOfBounds s (x, y) = x < 0 || y < 0 || x >= xLength s || y >= yLength s
xLength = length . head
yLength = length

get :: [String] -> (Int, Int) -> Char
get s (x, y) | isOutOfBounds s (x, y) = ' '
get s (x, y) = (s !! y) !! x

replace :: [String] -> (Int, Int) -> Char -> [String]
replace s (x, y) _ | isOutOfBounds s (x, y) = s
replace s (x, y) elem = let (first, second) = splitAt y s in 
    first ++ [replaceElement] ++ tail second
  where 
    replaceElement = let (first, second) = splitAt x (s !! y) in 
      first ++ [elem] ++ tail second
