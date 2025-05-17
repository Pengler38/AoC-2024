{-# LANGUAGE BangPatterns #-}

module Day11 (day11, day11p2) where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Tree as Tree
import qualified Data.Array as Array
import qualified Data.Map as Map
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State

import qualified Control.Monad.ST as ST
import qualified Data.STRef as STRef
import qualified Data.Array.ST as STArray
import qualified Data.Array.MArray as MArray
import qualified Data.HashTable.ST.Basic as HashTable

type RulesMap = Map.Map (Int, Int) Int

parse :: [String] -> [Int]
parse ss =
  let s = List.words $ head ss in
  map read s



day11 :: [String] -> String
day11 = show . solveHash 25

day11p2 :: [String] -> String
day11p2 = show . solveHash 75

-- ----------------------------------------
type RulesHash s = HashTable.HashTable s (Int, Int) Int

runRulesHash :: Int -> Int -> RulesHash s -> ST.ST s Int
runRulesHash !rulesLeft !n rulesHash = do
  let
    strN = show n
    rulesLeft' = rulesLeft - 1
    run = do
      result <- if n == 0 then -- Rule 1
          runRulesHash rulesLeft' 1 rulesHash
        else if even $ length strN then do -- Rule 2
          let (firstHalf, secondHalf) = List.splitAt (length strN `div` 2) strN
          r1 <- runRulesHash rulesLeft' (read firstHalf) rulesHash
          r2 <- runRulesHash rulesLeft' (read secondHalf) rulesHash
          return (r1 + r2)
        else -- Rule 3
          runRulesHash rulesLeft' (n * 2024) rulesHash
      HashTable.insert rulesHash (n, rulesLeft) result
      return result
  if rulesLeft == 0 then
    return 1
  else do
    m <- HashTable.lookup rulesHash (n, rulesLeft)
    maybe run return m

solveHash :: Int -> [String] -> Int
solveHash times s =
  let ints = parse s in
  ST.runST $ do
    ref <- HashTable.new
    Monad.foldM (\acc n -> runRulesHash times n ref >>= \res -> return $ acc + res) 0 ints
  
-- ----------------------------------------
-- Fastest solution I could think of using only base packages, but still quite slow due to the speed of Map lookups
-- It may be possible to make a solution with an Array...?
runRulesMap :: Int -> Int -> State.State RulesMap Int
runRulesMap !rulesLeft !n = do
  rulesMap <- State.get
  let
    strN = show n
    rulesLeft' = rulesLeft - 1
    run = do
      result <- if n == 0 then -- Rule 1
          runRulesMap rulesLeft' 1
        else if even $ length strN then do -- Rule 2
          let (firstHalf, secondHalf) = List.splitAt (length strN `div` 2) strN
          r1 <- runRulesMap rulesLeft' (read firstHalf)
          r2 <- runRulesMap rulesLeft' (read secondHalf)
          return (r1 + r2)
        else -- Rule 3
          runRulesMap rulesLeft' (n * 2024)
      State.put $ Map.insert (n, rulesLeft) result rulesMap
      return result
  if rulesLeft == 0 then
    return 1
  else
    maybe run return (Map.lookup (n, rulesLeft) rulesMap)

solveMap :: Int -> [String] -> Int
solveMap times =
  (`State.evalState` mempty)
  . Monad.foldM (\acc n -> runRulesMap times n >>= \res -> return $ acc + res) 0
  . parse


-- -----------------------------------
-- Faster compiled than solveTree, slower than solveMap
solveRec :: Int -> [String] -> Int
solveRec times =
  let
    run !rulesLeft !n =
      let strN = show n in
      if rulesLeft == 0 then
        1
      else if n == 0 then
        run (rulesLeft-1) 1
      else if even $ length strN then
        let (firstHalf, secondHalf) = List.splitAt (length strN `div` 2) strN in
        run (rulesLeft-1) (read firstHalf) + run (rulesLeft-1) (read secondHalf)
      else
        run (rulesLeft-1) (n*2024)
  in
  foldl (\acc n -> acc + run times n) 0
  . parse

-- -----------------------------------
-- Slower compiled, but pretty fast interpreted likely due to most of the work
-- being done in Data.Tree
runRulesTree :: Int -> Tree.Tree Int
runRulesTree !int =
  let str = show int in
  if int == 0
    then pure 1
  else if even $ length str
    then let (firstHalf, secondHalf) = List.splitAt (length str `div` 2) str in
    Tree.Node (read firstHalf) [pure $ read secondHalf]
  else
    pure (int * 2024)

parseTree :: [String] -> Tree.Tree Int
parseTree s =
  let
    ints :: [Int]
    ints = map read $ List.words $ head s
  in
  Tree.Node (head ints) (map pure $ tail ints)

solveTree :: Int -> [String] -> Int
solveTree x =
  let
    doNTimes n f = last . take (n+1) . iterate f
    treeLength = length . Tree.flatten
  in
  treeLength . doNTimes x (>>= runRulesTree) . parseTree

