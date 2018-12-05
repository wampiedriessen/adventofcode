module Solutions.Day05
( solvers
) where

import CommonHelpers
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Char as C

solvers = [solveP1,solveP2]

comp :: Char -> Char -> Bool
comp a b = (C.isLower a && C.toUpper a == b) || (C.isLower b && C.toUpper b == a)

reactUnits :: Char -> [Char] -> [Char]
reactUnits x [] = [x]
reactUnits x (y:acc) =
    if comp x y then
        acc
    else
        x:y:acc

fullyReact :: [Char] -> [Char]
fullyReact = foldr reactUnits []

solveP1 :: [String] -> String
solveP1 = show . length . fullyReact . head

-- || Start Part 2

removeUnits :: Char -> [Char] -> [Char]
removeUnits ban = filter (not . (`elem` [ban, C.toUpper ban]))

solveP2 :: [String] -> String
solveP2 input =
    let
        polymer = head input
        possibleUnits = L.nub $ map C.toLower polymer
    in show $ minimum $ map (length . fullyReact . flip removeUnits polymer) possibleUnits