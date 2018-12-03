import Solutions.Day3
import System.IO

import CommonHelpers

main = do
    input <- getContents
    -- input <- readFile "Inputs/day1.txt"
    let problem = lines input
    putStr "\n"
    putStr $ "Deel 1: " ++ solveP1 problem
    putStr "\n"
    putStr $ "Deel 2: " ++ solveP2 problem
    putStr "\n"
