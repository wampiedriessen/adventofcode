import qualified Solutions.Day01 as D01
import qualified Solutions.Day02 as D02
import qualified Solutions.Day03 as D03
import qualified Solutions.Day04 as D04
import qualified Solutions.Day05 as D05
import qualified Solutions.Day06 as D06
import qualified Solutions.Day07 as D07
import qualified Solutions.Day08 as D08
import qualified Solutions.Day09 as D09
import qualified Solutions.Day10 as D10
import qualified Solutions.Day11 as D11
import qualified Solutions.Day12 as D12
import qualified Solutions.Day13 as D13
import qualified Solutions.Day14 as D14
import qualified Solutions.Day15 as D15
import qualified Solutions.Day16 as D16
import qualified Solutions.Day17 as D17
import qualified Solutions.Day18 as D18
import qualified Solutions.Day19 as D19
import qualified Solutions.Day20 as D20
import qualified Solutions.Day21 as D21
import qualified Solutions.Day22 as D22
import qualified Solutions.Day23 as D23
import qualified Solutions.Day24 as D24
import qualified Solutions.Day25 as D25

import System.CPUTime
import System.Environment
import System.IO
import Text.Printf

import CommonHelpers

solutions = [D01.solvers, D02.solvers, D03.solvers, D04.solvers, D05.solvers, D06.solvers, D07.solvers, D08.solvers, D09.solvers, D10.solvers, D11.solvers, D12.solvers, D13.solvers, D14.solvers, D15.solvers, D16.solvers, D17.solvers, D18.solvers, D19.solvers, D20.solvers, D21.solvers, D22.solvers, D23.solvers, D24.solvers, D25.solvers]

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
