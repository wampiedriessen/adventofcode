module Solutions.Day07
( solvers
) where

import CommonHelpers
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

solvers = [solveP1,solveP2]

-- Step Z must be finished before step H can begin.
parse :: String -> (String,S.Set String)
parse x =
    let split = words x
    in (split !! 1, S.fromList [split !! 7])

parseDep :: String -> (String,S.Set String)
parseDep x =
    let split = words x
    in (split !! 7, S.fromList [split !! 1])

firstInSet = head . S.toList
lastInSet = last . S.toList

findTargets :: String -> M.Map String (S.Set String) -> S.Set String
findTargets key graph = case M.lookup key graph of
        Nothing -> S.empty
        Just x -> x

-- recurse1 :: S.Set String -> String -> M.Map String [String] -> String
-- recurse1 todo first graph =
--     let newTargets = S.union (S.delete first todo) $ S.fromList $ findTargets first graph
--     in if S.null newTargets then first
--         else first ++ (recurse1 (tail newStack) (head newStack) graph)

targetsToGo :: String -> M.Map String (S.Set String) -> S.Set String
targetsToGo from graph = S.insert from $ S.foldl (\acc x -> S.union acc $ targetsToGo x graph) S.empty $ findTargets from graph

firstEligibleInSet :: S.Set String -> M.Map String (S.Set String) -> String
firstEligibleInSet targets graph =
    let
        blocked = S.foldl S.union S.empty $ S.map (\x -> S.delete x $ targetsToGo x graph) targets
    in head $ filter (not . flip S.member blocked) $ S.toList targets

recurse1 :: S.Set String -> M.Map String (S.Set String) -> String
recurse1 targets graph =
    let
        top = firstEligibleInSet targets graph
        newTargets = S.delete top $ S.union targets $ targetsToGo top graph
    in if S.null newTargets then top
        else top ++ (recurse1 newTargets graph)

solveP1 :: [String] -> String
solveP1 x =
    let
        input = map parse x
        graph = M.fromListWith (\n1 n2 -> S.union n1 n2) $ input
        targets = foldl (S.union) S.empty $ M.elems graph
        first = S.fromList $ L.nub $ map fst $ filter (not . (flip S.member targets) . fst) input
    in recurse1 first graph

-- || Start Part 2

solveP2 :: [String] -> String
solveP2 x = "ToDo: part 2"