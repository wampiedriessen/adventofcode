module Solutions.Day11
( solvers
) where

import CommonHelpers
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Function (on)

solvers = [solveP1,solveP2]

type Coordinate = (Int,Int)

powerlevel :: Int -> Int -> Int -> Int
powerlevel input x y =
    let rId = (x+10)
        subAns = ((rId*y)+input)*rId
        ans = (quot (subAns `mod` 1000) 100) - 5
    in ans

createSumGridMiddle input grid [] = grid
createSumGridMiddle input grid ((x,y):rest) =
    let gridL = unwrapInt . (flip M.lookup grid)
        power = powerlevel input x y
        powerSum = power + (gridL (x,y-1)) + (gridL (x-1,y)) - (gridL (x-1,y-1))
        newGrid = M.insert (x,y) powerSum grid
    in createSumGridMiddle input newGrid rest

createSumGrid input width =
    let powlev = uncurry (powerlevel input)
        first = powlev (1,1)
        topEdge = [(x,1) | x <- [2..width]]
        leftEdge = [(1,y) | y <- [2..width]]
        topVals = scanl (\acc x -> acc + (powlev x)) first topEdge
        leftVals = scanl (\acc x -> acc + (powlev x)) first leftEdge
        edgeCases = M.union (M.fromList $ zip ((1,1):topEdge) topVals) (M.fromList $ zip ((1,1):leftEdge) leftVals)
        middles = [(x,y) | x <- [2..width], y <- [2..width]]
    in createSumGridMiddle input edgeCases middles

getAreaPower grid size (x,y) =
    let
        z = size-1
        xA = unwrapInt $ M.lookup (x-1,y-1) grid
        xB = unwrapInt $ M.lookup (x+z,y-1) grid
        xC = unwrapInt $ M.lookup (x-1,y+z) grid
        xD = unwrapInt $ M.lookup (x+z,y+z) grid
    in xD + xA - xB - xC

solveForSize grid size =
    let coords = [(x,y) | x <- [1..(301-size)], y <- [1..(301-size)]]
        totalPower = getAreaPower grid size
        maxCoord = L.maximumBy (compare `on` totalPower) coords
    in (maxCoord, totalPower maxCoord)

solve1 x =
    let sumgrid = createSumGrid x 300
    in show $ fst $ solveForSize sumgrid 3

solveP1 :: [String] -> String
solveP1 = head . map (solve1 . read)

-- || Start Part 2

showAns (((x,y),_),z) = (show x) ++ "," ++ (show y) ++ "," ++ (show z)

solve2 x =
    let sumgrid = createSumGrid x 300
        levelsBests = zip (map (solveForSize sumgrid) [3..300]) [3..300]
    in showAns $ L.maximumBy (compare `on` (snd . fst)) levelsBests

solveP2 :: [String] -> String
solveP2 = head . map (solve2 . read)