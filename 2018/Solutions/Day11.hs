module Solutions.Day11
( solvers
) where

import CommonHelpers
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Function (on)

solvers = [solveP1,solveP2]

type CoordLevel = (Int,Int,Int)

powerlevel :: Int -> Int -> Int -> Int
powerlevel input x y =
    let rId = (x+10)
        subAns = ((rId*y)+input)*rId
        ans = (quot (subAns `mod` 1000) 100) - 5
    in ans

getEdges size x y =
    [(x+(size-1),y') | y' <- [y..(y+(size-1))]] ++ [(x',y+(size-1)) | x' <- [x..(x+(size-2))]]

powerLevelDepth :: (M.Map CoordLevel Int) -> Int -> Int -> Int -> Int
powerLevelDepth grid size x y =
    if size == 1 then unwrapInt $ M.lookup (x,y,1) grid
        else let
            lowerGridSize = unwrapInt $ M.lookup (x,y,size-1) grid
            edges = getEdges size x y
            sumEdges = sum $! map (uncurry (powerLevelDepth grid 1)) edges
        in sumEdges + lowerGridSize

newPowerLevels :: (M.Map CoordLevel Int) -> Int -> [(Int,Int)] -> [(CoordLevel,Int)]
newPowerLevels grid level = map (\(x,y) -> (((x,y,level) :: CoordLevel),powerLevelDepth grid level x y))

createGrids :: (M.Map CoordLevel Int) -> Int -> ((M.Map CoordLevel Int),(CoordLevel,Int))
createGrids grid level =
    if level == 1 then (grid,((1,1,1),0))
        else let
            (lowerGrid,highest) = createGrids grid (level-1)
            coordsForLevel = [(x,y) | x <- [1..(301-level)], y <- [1..(301-level)]]
            newPowers = newPowerLevels lowerGrid level coordsForLevel
            newHighest = L.maximumBy (compare `on` snd) (highest:newPowers)
        in (M.union grid $ M.fromList newPowers, newHighest)

showAns :: CoordLevel -> String
showAns (x,y,z) = (show x) ++ "," ++ (show y) ++ "," ++ (show z)

solve1 x =
    let powlev = uncurry (powerlevel x)
        grid = M.fromList [((x,y,1),powlev (x,y)) | x <- [1..300], y <- [1..300]]
        powergrid = createGrids grid 3
    in showAns $ fst $ snd $ powergrid

solveP1 :: [String] -> String
solveP1 = ('\n':) . unlines . map (solve1 . read)

-- || Start Part 2

solve2 x =
    let powlev = uncurry (powerlevel x)
        grid = M.fromList [((x,y,1),powlev (x,y)) | x <- [1..300], y <- [1..300]]
        powergrid = createGrids grid 300
    in showAns $ fst $ snd $ powergrid

solveP2 :: [String] -> String
solveP2 = ('\n':) . unlines . map (solve2 . read)