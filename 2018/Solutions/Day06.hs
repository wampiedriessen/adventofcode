module Solutions.Day06
( solvers
) where

import CommonHelpers
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

solvers = [solveP1,solveP2]

data Claim = Coord { x :: Int, y :: Int } | Nada | Tie deriving (Eq, Ord, Show)

getX (Coord x _) = x
getY (Coord _ y) = y

readC :: String -> Claim
readC = tup . map read . words . L.delete ','
    where tup (x:y:_) = Coord x y

mDistance :: Claim -> Claim -> Int
mDistance (Coord x1 y1) (Coord x2 y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

closestNode :: Claim -> [Claim] -> (Claim,Int)
closestNode _ [] = (Nada,0)
closestNode x (f:coords) =
    let
        best = closestNode x coords
        cur = mDistance x f
    in case fst best of
        Nada -> (f, cur)
        Tie -> if cur < (snd best) then (f, cur) else best
        Coord _ _ -> if cur > (snd best)
            then best
            else if cur == (snd best)
                then (Tie,cur)
                else (f,cur)

calcOuterCoords coords =
    let
        xRange = [(minimum $ map getX coords)..(maximum $ map getX coords)]
        yRange = [(minimum $ map getY coords)..(maximum $ map getY coords)]
        box = [map (fst . flip closestNode coords) [Coord x y | x <- xRange] | y <- yRange]
    in (S.fromList $ concat [box !! 0, box !! ((length box) - 1), map head box, map last box], box, xRange, yRange)

unwrap :: Maybe Int -> Int
unwrap x = case x of
        Just x -> x
        Nothing -> -1

solveP1 :: [String] -> String
solveP1 x =
    let
        coords = map readC x
        (outerNodes,grid,_,_) = calcOuterCoords coords
        innerCoords = filter (not . (flip S.member outerNodes)) coords
    in show $ countMostPrevalent $ filter (`elem` innerCoords) $ concat grid

-- || Start Part 2

sumDistances x = sum . map (mDistance x)

solveP2 :: [String] -> String
solveP2 x =
    let
        coords = map readC x
        (_,_,xRange,yRange) = calcOuterCoords coords
        totDistances = map (flip sumDistances coords) [Coord x y | x <- xRange, y <- yRange]
    in show $ length $ filter (<10000) $ totDistances