module Solutions.Day03
( solvers
) where

import CommonHelpers
import qualified Data.Map as M

solvers = [solveP1,solveP2]

-- #1 @ 1,3: 4x4
-- left,top,width,height

parse :: String -> (String,[(Int,Int)])
parse claim =
    let
        key = tail $ takeWhile (/=' ') claim
        left = read $ takeWhile (/=',') $ tail $ dropWhile (/='@') claim
        top = read $ takeWhile (/=':') $ tail $ dropWhile (/=',') claim
        width = read $ takeWhile (/='x') $ tail $ dropWhile (/=':') claim
        height = read $ tail $ dropWhile (/='x') claim
    in (key,[(x,y) | x <- [left..(left+width-1)], y <- [top..(top+height-1)]])

countSquaresClaimed :: (Ord a, Num b) => [a] -> M.Map a b
countSquaresClaimed = M.fromListWith (\n1 n2 -> n1 + n2) . flip zip (cycle [1])

solveP1 :: [String] -> String
solveP1 = show . M.size . M.filter (>1) . countSquaresClaimed . concat . map (snd . parse)

-- || Start Part 2

solveP2 :: [String] -> String
solveP2 x =
    let
        claimLists = map parse x
        doubleClaimedSquares = M.filter (>1) $ countSquaresClaimed $ concat $ map (snd . parse) x
    in fst $ head $ filter (all (not . flip M.member doubleClaimedSquares) . snd) claimLists
