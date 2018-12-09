module Solutions.Day09
( solvers
) where

import CommonHelpers
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Sequence as S

solvers = [solveP1,solveP2]

parse :: String -> (Int,Int)
parse x =
    let split = words x
        players = read $ split !! 0
        marbles = read $ split !! 6
    in (players, marbles)

getRelindex :: (S.Seq Int) -> Int -> Int -> Int
getRelindex board cur steps =
    mod (cur+steps) $ S.length board

solve :: Int -> (S.Seq Int) -> Int -> [Int] -> M.Map Int Int
solve _ _ _ [] = M.empty
solve n b cur (m:todo) = if mod m 23 == 0
        then let
            newCur = getRelindex b cur (-7)
            goalPoints = (b `S.index` newCur)
            newBoard = S.deleteAt newCur b
        in M.insertWith (+) (mod m n) (goalPoints+m) $ solve n newBoard newCur todo
    else let
        newCur = getRelindex b cur 2
        newBoard = S.insertAt newCur m b
        in solve n newBoard newCur todo

baseSolve :: Int -> [String] -> String
baseSolve multiplier x =
    let problems = map parse x
        getscores (n,m) = solve n (S.fromList [0]) 0 [1..(multiplier*m)]
        gethighscore = maximum . map snd . M.toList . getscores
    in show $ map gethighscore problems

solveP1 :: [String] -> String
solveP1 = baseSolve 1

-- || Start Part 2

solveP2 :: [String] -> String
solveP2 = baseSolve 100