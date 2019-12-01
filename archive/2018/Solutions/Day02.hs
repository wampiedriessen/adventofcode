module Solutions.Day02
( solvers
) where

import CommonHelpers
import qualified Data.List as L

solvers = [solveP1,solveP2]

hasTwo :: [[Char]] -> Bool
hasTwo = any ((2 ==) . length)

hasThree :: [[Char]] -> Bool
hasThree = any ((3 ==) . length)

recSolve1 [] = (0,0)
recSolve1 (tag:tags) =
    let
        (twos,threes) = recSolve1 tags
        plus2 = if hasTwo tag then 1 else 0
        plus3 = if hasThree tag then 1 else 0
    in (twos+plus2,threes+plus3)

solveP1 :: [String] -> String
solveP1 = show . uncurry (*) . recSolve1 . map (L.group . L.sort)

-- || Start Part 2

createAllPairs :: [a] -> [(a, a)]
createAllPairs l = [(x,y) | (x:ys) <- L.tails l, y <- ys]

hasOneDiff :: Eq a => ([a], [a]) -> Bool
hasOneDiff (_,[]) = False
hasOneDiff ([],_) = False
hasOneDiff (x:xs,y:ys) = if x /= y then xs == ys else hasOneDiff (xs,ys)

rmFirstAnomaly :: Eq a => ([a], [a]) -> [a]
rmFirstAnomaly (_,[]) = []
rmFirstAnomaly ([],_) = []
rmFirstAnomaly (x:xs,y:ys) = if x == y then x:(rmFirstAnomaly (xs,ys)) else xs

solveP2 :: [String] -> String
solveP2 = rmFirstAnomaly . head . filter hasOneDiff . createAllPairs
