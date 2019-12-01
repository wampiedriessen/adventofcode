module Solutions.Day07
( solvers
) where

import CommonHelpers
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Function (on)

solvers = [solveP1,solveP2,solveP2Test]

-- Step Z must be finished before step H can begin.
parseDep :: String -> (String,S.Set String)
parseDep x =
    let split = words x
    in (split !! 7, S.fromList [split !! 1])

findTargets :: String -> M.Map String (S.Set String) -> S.Set String
findTargets key graph = case M.lookup key graph of
        Nothing -> S.empty
        Just x -> x

getFreeDeps :: S.Set String -> S.Set String -> M.Map String (S.Set String) -> S.Set String
getFreeDeps todo done graph = S.filter (S.null . flip S.difference done . flip findTargets graph) todo

recurse1 :: S.Set String -> S.Set String -> M.Map String (S.Set String) -> String
recurse1 todo done graph = if S.null todo then ""
    else let
        firstNew = head $ S.toList $ getFreeDeps todo done graph
        newDone = S.insert firstNew done
        newTodo = S.delete firstNew todo
    in firstNew ++ recurse1 newTodo newDone graph

solveP1 :: [String] -> String
solveP1 x =
    let
        input = map parseDep x
        graph = M.fromListWith (\n1 n2 -> S.union n1 n2) $ input
        values = foldl (S.union) S.empty $ M.elems graph
        keys = S.fromList $ M.keys graph
        all = S.union keys values
    in recurse1 all S.empty graph

-- || Start Part 2

recurse2 :: Int -> ([String] -> [(String,Int)]) -> S.Set String -> [(String,Int)] -> S.Set String -> M.Map String (S.Set String) -> Int
recurse2 nrOfWorkers getDurations todo busy done graph = if S.null todo then 0
    else let
        freeSlots = nrOfWorkers - (length busy)
        busyNodes = S.fromList $ map (fst) busy
        newNodes = take freeSlots $ S.toList $ (getFreeDeps todo done graph) S.\\ busyNodes
        ((node,time):tmpBusy) = L.sortBy (compare `on` snd) $ busy ++ getDurations newNodes

        newBusy = map (\(n,x) -> (n,x-time)) tmpBusy

        newDone = S.insert node done
        newTodo = S.delete node todo
    in time + recurse2 nrOfWorkers getDurations newTodo newBusy newDone graph

solveP2' :: Int -> ([String] -> [(String,Int)]) -> [String] -> String
solveP2' nrOfWorkers durationFun x =
    let
        input = map parseDep x
        graph = M.fromListWith (\n1 n2 -> S.union n1 n2) $ input
        values = foldl (S.union) S.empty $ M.elems graph
        keys = S.fromList $ M.keys graph
        all = S.union keys values
    in show $ recurse2 nrOfWorkers durationFun all [] S.empty graph

tTestLookup = M.fromList $ zip (map (:"") ['A'..]) [1,2..]
tRealLookup = M.fromList $ zip (map (:"") ['A'..]) [61,62..]

solveP2 :: [String] -> String
solveP2 x = let
        getLongDurations = map (\x -> (x,unwrapInt $ M.lookup x tRealLookup))
    in solveP2' 5 getLongDurations x

solveP2Test :: [String] -> String
solveP2Test x = let
        getShortDurations = map (\x -> (x,unwrapInt $ M.lookup x tTestLookup))
    in solveP2' 2 getShortDurations x