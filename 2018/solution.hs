import qualified Solutions.Day1 as D1
import qualified Solutions.Day2 as D2
import qualified Solutions.Day3 as D3
import qualified Solutions.Day4 as D4
import qualified Solutions.Day5 as D5

import System.CPUTime
import System.Environment
import System.IO
import Text.Printf

import CommonHelpers

solutions = [D1.solvers, D2.solvers, D3.solvers, D4.solvers, D5.solvers]

main = do

    args <- getArgs

    let pNr = head args
    let isTest = length args == 2
    let iType = if isTest then "test" else "d"
    let fileName = "Inputs/" ++ iType ++ pNr ++ ".txt"

    handle <- openFile fileName ReadMode
    input <- hGetContents handle

    let solveP1 = (solutions !! (read pNr - 1)) !! 0;
    let solveP2 = (solutions !! (read pNr - 1)) !! 1;

    let problem = lines input

    putStr "\n"

    start1 <- getCPUTime
    putStr $ "Deel 1: " ++ solveP1 problem
    end1 <- getCPUTime
    putStr "\n"

    start2 <- getCPUTime
    putStr $ "Deel 2: " ++ solveP2 problem
    end2 <- getCPUTime
    putStr "\n"

    let d1 = (fromIntegral (end1 - start1)) / (10^12)
    let d2 = (fromIntegral (end2 - start2)) / (10^12)

    printf "Timings - 1: %0.3f sec, 2: %1.3f sec\n" (d1 :: Double) (d2 :: Double)
    putStr "\n"
