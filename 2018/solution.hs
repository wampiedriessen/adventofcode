import Solutions.Day1
import System.IO

import CommonHelpers

main = do
    input <- getContents
    -- input <- readFile "Inputs/day1.txt"
    let problem = lines input
    putStr "\n"
    putStr $ solveP1 problem
    putStr "\n"
    putStr $ solveP2 problem
    putStr "\n"
