module Solutions.Day02
( solvers
) where

import CommonHelpers
import qualified Data.List as L

solvers = [solveP1,solveP2]

hasTwo :: String -> Bool
hasTwo = not . L.null . filter ((2 ==) . length) . L.group . L.sort

hasThree :: String -> Bool
hasThree = not . L.null . filter ((3 ==) . length) . L.group . L.sort

solveP1 :: [String] -> String
solveP1 x =
    let
        counters = foldr (\tag (twos,threes) -> (twos + (if hasTwo tag then 1 else 0), threes + (if hasThree tag then 1 else 0))) (0,0) x
    in show $ (fst counters) * (snd counters)

-- || Start Part 2

createAllPairs :: [a] -> [(a, a)]
createAllPairs [] = []
createAllPairs [x] = []
createAllPairs (x:xs) = foldl (\acc y -> (x,y):acc) [] xs ++ createAllPairs xs

hasNoDiff :: Eq a => ([a], [a]) -> Bool
hasNoDiff (_,[]) = True
hasNoDiff ([],_) = True
hasNoDiff (x:xs,y:ys) = if x /= y then False else hasNoDiff (xs,ys)

hasOneDiff :: Eq a => ([a], [a]) -> Bool
hasOneDiff (_,[]) = False
hasOneDiff ([],_) = False
hasOneDiff (x:xs,y:ys) = if x /= y then hasNoDiff (xs,ys) else hasOneDiff (xs,ys)

removeUncompared :: Eq a => ([a], [a]) -> [a]
removeUncompared (_,[]) = []
removeUncompared ([],_) = []
removeUncompared (x:xs,y:ys) = if x == y then x:(removeUncompared (xs,ys)) else removeUncompared (xs,ys)

solveP2 :: [String] -> String
solveP2 = removeUncompared . head . filter hasOneDiff . createAllPairs
