module Solutions.Day1
( solveP1
, solveP2
) where

import CommonHelpers

followers :: [Int] -> [Int]
followers [] = []
followers (x:[]) = []
followers (x:y:xs) = if x == y
    then x:followers (y:xs)
    else followers (y:xs)

allfollowers :: [Int] -> [Int]
allfollowers [] = []
allfollowers (x:[]) = []
allfollowers (x:xs) = if x == (last xs)
    then x:(followers (x:xs))
    else followers (x:xs)

solveP1 :: [Int] -> Int
solveP1 = sum . allfollowers

-- || Start Part 2

allbalanced :: [Int] -> [Int] -> [Int]
allbalanced _ [] = []
allbalanced [] _ = []
allbalanced (x:xs) (y:ys) = if x == y
    then (2*x):(allbalanced xs ys)
    else allbalanced xs ys

-- allbalanced :: (Num a, Eq a) => [a] -> [a] -> [a]
-- allbalanced = zipWith (\a0 b0 -> if a0 == b0 then 2*a0 else 0)

solveP2 :: [Int] -> Int
solveP2 x =
    let s = splitInHalf x in sum (allbalanced (fst s) (snd s))