module Solutions.Day03
( solvers
) where

import CommonHelpers
import qualified Data.List as L

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


solveP1 :: [String] -> String
solveP1 = show . length . filterSingles . createGridItems
    where
        createGridItems = L.group . L.sort . concat . map (snd . parse)
        filterSingles = filter ((>=2) . length)

-- || Start Part 2

solveP2 :: [String] -> String
solveP2 x =
    let
        gridItems = L.sort $ concat $ map (snd . parse) x
        isSingleInList grid x = 1 == (length $ filter (x==) grid)
        allSingleInList grid x = all (isSingleInList grid) x
    in fst $ head $ filter ((allSingleInList gridItems) . snd) $ map parse x
