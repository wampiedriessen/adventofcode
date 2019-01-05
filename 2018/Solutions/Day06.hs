module Solutions.Day06
( solvers
) where

import CommonHelpers
import qualified Data.List as L
import qualified Data.Set as S

solvers = [solveP1,solveP2]

data Claim = Patch { x :: Int, y :: Int } | Nada | Tie deriving (Eq, Ord, Show)

getPX (Patch x _) = x
getPY (Patch _ y) = y

readC :: String -> Claim
readC = tup . map read . words . L.delete ','
    where tup (x:y:_) = Patch x y

mDistance :: Claim -> Claim -> Int
mDistance (Patch x1 y1) (Patch x2 y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

closestNode :: Claim -> [Claim] -> (Claim,Int)
closestNode _ [] = (Nada,0)
closestNode x (f:coords) =
    let
        best = closestNode x coords
        cur = mDistance x f
    in case fst best of
        Nada -> (f, cur)
        Tie -> if cur < (snd best) then (f, cur) else best
        Patch _ _ -> if cur > (snd best)
            then best
            else if cur == (snd best)
                then (Tie,cur)
                else (f,cur)

calcOuterCoords coords =
    let
        xRange = [(minimum $ map getPX coords)..(maximum $ map getPX coords)]
        yRange = [(minimum $ map getPY coords)..(maximum $ map getPY coords)]
        box = [map (fst . flip closestNode coords) [Patch x y | x <- xRange] | y <- yRange]
        outerElements = S.fromList $ concat [box !! 0, box !! ((length box) - 1), map head box, map last box]
    in (outerElements, box, xRange, yRange)

solveP1 :: [String] -> String
solveP1 x =
    let
        coords = map readC x
        (outerNodes,grid,_,_) = calcOuterCoords coords
        innerCoords = filter (not . (flip S.member outerNodes)) coords
    in show $ countMostPrevalent $ filter (`elem` innerCoords) $ concat grid

-- || Start Part 2

solveP2 :: [String] -> String
solveP2 x = concat ["test: ", solveP2' True x," real: ", solveP2' False x]

solveP2' :: Bool -> [String] -> String
solveP2' isTest x =
    let
        limit = if isTest then 32 else 10000
        coords = map readC x
        (_,_,xRange,yRange) = calcOuterCoords coords
        sumDistances x = sum . map (mDistance x)
        totDistances = map (flip sumDistances coords) [Patch x y | x <- xRange, y <- yRange]
    in show $ length $ filter (<limit) $ totDistances