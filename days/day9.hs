--day9.hs
--Preston Engler

--Yeah, I know this totally blows
--The readability/complexity, number of lines, and speed are all terrible
--But it does work
--I'm going to have to look up how other people did it

{-# LANGUAGE PatternSynonyms #-}

module Day9 where

import qualified Data.Map as Map
import Data.Sequence ((<|), (|>), (><), pattern (:<|), pattern (:|>))
import qualified Data.Sequence as S
import Debug.Trace

--import Data.Array.MArray as A -- ?
--import Data.Sequence

--type Arr = A.MArray

type Map = Map.Map
type Seq = S.Seq


day9 :: [String] -> String 
day9 = show . checksum . compress . parse . head

parse :: String -> Map Int Int
parse s = buildMap 0 0 s
  where 
    insertMultiple index id 0 map = map :: Map Int Int
    insertMultiple index id count map = Map.insert index id $ insertMultiple (index+1) id (count-1) map
    buildMap index id (a:b:xs) = insertMultiple index id (read [a]) $ buildMap (index + read [a] + read [b]) (id+1) xs
    buildMap index id [a] = insertMultiple index id (read [a]) Map.empty

compress :: Map Int Int -> Map Int Int
compress m = let maxKey = fst $ Map.findMax m in 
  compress' 0 maxKey 
    where
      compress' firstPos lastPos
        | firstPos >= lastPos = m
        | otherwise = case (firstPos `Map.lookup` m, lastPos `Map.lookup` m) of
          (Nothing, Just a) -> Map.insert firstPos a $ Map.delete lastPos $ compress' (firstPos+1) (lastPos-1)
          (Just _, Just a) -> compress' (firstPos+1) lastPos
          (Just _, Nothing) -> compress' (firstPos+1) (lastPos-1)
          (Nothing, Nothing) -> compress' firstPos (lastPos-1)
      

checksum :: Map Int Int -> Int
checksum m = Map.foldrWithKey checksum' 0 m
  where 
    checksum' index id acc = acc + index*id

-- :: Map Int Int -> Map Int Int

--For debugging:
mapToString :: Map Int Int -> String 
mapToString m = mapToString' 0
  where 
    maxKey = fst $ Map.findMax m
    mapToString' :: Int -> String
    mapToString' id 
      | id <= maxKey = case Map.lookup id m of
        Just a -> (head . show) a : mapToString' (id+1)
        Nothing -> '.' : mapToString' (id+1)
      | otherwise = []


day9p2 :: [String] -> String 
--day9p2 = show . checksump2 0 . compressp2 S.empty . S.fromList . parsep2 0 0 . head
day9p2 = show . checksump2 0 . compressp2 S.empty . S.fromList . parsep2 0 0 . head

--From String to index, length. ID is (initially) the index.
parsep2 :: Int -> Int -> String -> [(Int, Int, Int)]
parsep2 idx id (a:b:str) = (idx, id, blockLength) : parsep2 (idx + blockLength + emptyLength) (id+1) str
  where 
    blockLength = read [a]
    emptyLength = read [b]
parsep2 idx id [a] = [(idx, id, read [a])]

--WHENEVER YOU COME ACROSS A GAP, SCAN FROM THE RIGHT (UNTIL YOU HIT THE CURRENT POS) AND SEE IF IT ANY CAN FIT
--IF SO, THEN STICK IT IN THERE ALL SNUG AND THEN 
--IF NOT, THEN IT THAT LENGTH GOES INTO THE map DOOMED TO NEVER BE CHECKED AGAIN
  --Map Int Int ->

compressp2 :: Seq (Int, Int, Int) -> Seq (Int, Int, Int) -> Seq (Int, Int, Int)
--compressp2 accList (seq) | trace (show seq ++ "   " ++ show accList) False = undefined
--compressp2 accList (seq) | trace (show $ S.length seq) False = undefined
compressp2 accList (seq :|> a) = 
  let 
    (a_idx, a_id, a_len) = a 

    findGapIndex idx (b :<| c :<| s) = 
      let 
        (b_idx, _, b_len)= b 
        (c_idx, _, _)= c
        gap_len = c_idx - (b_idx + b_len) 
      in if (a_len <= gap_len) then (Just (idx+1), b_idx+b_len) else findGapIndex (idx+1) (c <| s)
    findGapIndex idx s = (Nothing, 0) --Base case

    gap_idx = findGapIndex 0 (seq |> a) --Has to have a appended to check gap between prev and current position!
  in
    case gap_idx of 
      (Just idx, newIdx) -> let 
          newElem = (newIdx, a_id, a_len)
        in compressp2 accList $ S.insertAt idx newElem seq
      (Nothing, _) -> compressp2 (a <| accList) (seq)

compressp2 accList seq = seq >< accList

checksump2 :: Int -> Seq (Int, Int, Int) -> Int
checksump2 acc (S.Empty) = acc
checksump2 acc ((a_idx, a_id, a_len) :<| seq) = 
  let 
    newAcc = acc + (sum $ take a_len $ zipWith (*) [a_idx..] (repeat a_id))
  in checksump2 newAcc seq

{-
showTheSequence seq = step [] seq
  where 
    step ls seq = ls 
    -}

{- Incorrect, iterates from the left forwards, doesn't make quite the right behavior
 
compressp2 :: Seq (Int, Int, Int) -> Seq (Int, Int, Int) -> Seq (Int, Int, Int)
compressp2 accList (a :<| b :<| seq) = 
  let 
    (a_idx, a_id, a_len) = a 
    (b_idx, b_id, b_len) = b; 
    gap = b_idx - (a_idx + a_len)
    movable_block_idx = S.findIndexR (\(_, _, l) -> l <= gap ) seq
  in
    if gap == 0 then compressp2 (accList |> a) (b <| seq)
    else --gap detected! bring out the big guns!
      case movable_block_idx of 
        Just idx -> 
          let 
            (c_idx, c_id, c_len) = seq `S.index` idx
            newElem = (a_idx+a_len, c_id, c_len)
          in compressp2 (accList |> a) (newElem <| b <| (S.deleteAt idx seq))
        Nothing -> compressp2 (accList |> a) (b <| seq)

compressp2 accList seq = accList >< seq
-}




{- graveyard of failed code below... 

--Going to do this one in an entirely different way than part 1 lol
day9p2 :: [String] -> String 
day9p2 = show . checksump2 0 . compressp2 . S.fromList . parsep2 . head

--Outputs an array of tuples (id, length)
parsep2 :: String -> [(Maybe Int, Int)]
parsep2 s = parsep2' 0 s
  where 
    parsep2' id (c1:c2:cs) = let len1 = read [c1] :: Int; len2 = read [c2] :: Int in
      (Just id, len1) : (Nothing, len2) : parsep2' (id+1) cs
    parsep2' id [c] = [(Just id, read [c] :: Int)]

compressp2 :: Seq (Maybe Int, Int) -> Seq (Maybe Int, Int)
compressp2 s = compressp2' 0 (S.length s - 1) s

compressp2' :: Int -> Int -> Seq (Maybe Int, Int) -> Seq (Maybe Int, Int)
--compressp2' firstpos lastpos s | trace (show firstpos ++ "  " ++ show lastpos ++ "  " ++ printSeq s) False = undefined
compressp2' startPositions firstpos lastpos s | trace (show lastpos) False = undefined
compressp2' startPositions firstPos lastPos s
  | lastPos == -1 = s
  | firstPos >= lastPos = compressp2' startPositions newFirstPosition newLastPosition s
    where 
      getNewLastPosition 0 = -1
      getNewLastPosition p = if s `S.index` (p-1) /= Nothing then p-1 else getNewLastPosition (p-1)
      newLastPosition = getNewLastPosition lastPos
      potentialFirstPosition = if newLastPosition == -1 then Just 0 else lookup (s `S.index` newLastPosition) startPositions
      newFirstPosition = case potentialFirstPosition of Nothing -> 0; Just a -> a
      --firstpos >= lastpos, this len should be removed from the startPositions pool by setting the value to the lastPos
      currentLength = snd $ s `S.index` lastPos
      newStartPositions = Map.insert currentLength lastPos startPositions

compressp2' startPositions firstPos lastPos s = case (s `S.index` firstPos, s `S.index` lastPos) of 
    ((Just _, _), (Just _, _)) -> compressp2' (firstPos+1) (lastPos) s
    --(_, (Nothing, _)) -> compressp2' 0 (lastPos-1) s
    ((Nothing, len1), (Just a, len2)) -> 
      if len1 > len2 then 
        compressp2' startPositions firstPos (lastPos+1) $ splitNothing len2 --splitNothing adds one element, add 1 to lastPos
      else if len1 == len2 then 
        let
          (partialS, numMerged1) = mergeAround lastPos $ swap firstPos lastPos 
          (newS, numMerged2) = mergeAround firstPos partialS
          newLastPosition = getNewLastPosition lastPos
          newStartPositions = Map.insert len2 firstPos startPositions
        in
          compressp2' newStartPositions 0!!!! (lastPos - (numMerged1+numMerged2)) newS
      else 
        compressp2' startPositions (firstPos+1) lastPos s
  where 
    swap i j = let iElem = s `S.index` i; jElem = s `S.index` j in
      S.update j iElem $ S.update i jElem $ s
    splitNothing len = let originalLen = snd $ s `S.index` firstPos in
      S.insertAt (firstPos+1) (Nothing, originalLen - len) $ S.update firstPos (Nothing, len) $ s

--merges all the Nothings, returns sequence and number merged so the position can be tracked properly
mergeAround :: Int -> Seq (Maybe Int, Int) -> (Seq (Maybe Int, Int), Int)
mergeAround idx s = (first >< merged >< end, numMerged)
  where 
    first = S.take (idx-1) s
    middle = S.take 3 $ S.drop (idx-1) s
    end = S.drop (idx+2) s
    (merged, numMerged) = merge 0 middle
    merge i (a :<| b :<| xs) = if fst a == Nothing && fst b == Nothing 
      then let (m, nm) = merge (i+1) ((Nothing, snd a + snd b) <| xs) in (m, nm)
      else let (m, nm) = merge i (b <| xs) in (a <| m, nm)
    merge i xs = (xs, i)

merge' :: Int -> Seq (Maybe Int, Int) -> (Seq (Maybe Int, Int), Int)
merge' 0 s = (s, 0)
merge' end (a :<| b :<| s) = if fst a == Nothing && fst b == Nothing 
  then let (mergedList, numMerged) = merge' (end-2) (s) in 
      ((Nothing, snd a + snd b) <| mergedList, numMerged+1)
  else let (mergedList, numMerged) = merge' (end-1) (b <| s) in 
      (a <| mergedList, numMerged)

--For debugging
printSeq :: Seq (Maybe Int, Int) -> String
printSeq (S.Empty) = []
printSeq ((maybeID, len) :<| s) = case maybeID of 
    Just id -> printx len (head $ show id) ++ printSeq s
    Nothing -> printx len '.' ++ printSeq s
  where 
    printx i = take i . repeat

--TODO may need to output Integer!
checksump2 :: Int -> Seq (Maybe Int, Int) -> Int
checksump2 idx (S.Empty) = 0
checksump2 idx ((maybeID, len) :<| s) = case maybeID of
  Nothing -> checksump2 (idx+len) s
  Just id -> (sum $ zipWith (*) indices (repeat id)) + checksump2 (idx+len) s
  where
    indices = take len [idx..]
-}
