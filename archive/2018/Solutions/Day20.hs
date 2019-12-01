module Solutions.Day20
( solvers
) where

import CommonHelpers
import qualified Data.List as L
import qualified Data.Set as S

solvers = [solveP1,solveP2]

apply c = case c of
    'N' -> above
    'S' -> below
    'E' -> rightOf
    'W' -> leftOf
    otherwise -> error "unkown input token"

applyTo c = S.map (apply c)
-- applyTo c = map (c:)

step (c:toDo, allPaths, splitSaver, pathStack) = case c of
    '(' -> (toDo, allPaths, allPaths:splitSaver, S.empty:pathStack) -- current paths on the splitSaver
    '|' -> (toDo, head splitSaver, splitSaver, (S.union allPaths $ head pathStack):(tail pathStack)) -- reset allPaths to top of splitSaver, add built path(s) in this option to the pathStack
    ')' -> (toDo, S.union allPaths $ head pathStack, tail splitSaver, tail pathStack) -- dump head of splitSaver, current allPaths is head of pathStack
    otherwise -> (toDo, applyTo c allPaths, splitSaver, pathStack)

walkAllPaths x = head $ dropWhile (\(x, _, _, _) -> x /= "$") $ iterate step (x, S.singleton (Point 0 0), [], [])

getEndCoords (_, x, _, _) = x

-- lengthOfLongest = last . L.sort . map length

solveP1 :: [String] -> String
solveP1 = show . getEndCoords . walkAllPaths . tail . head

-- || Start Part 2

solveP2 :: [String] -> String
solveP2 x = "ToDo: part 2"