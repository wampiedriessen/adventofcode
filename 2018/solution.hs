import Solutions.Day2

import System.CPUTime
import System.IO
import Text.Printf

import CommonHelpers

main = do
    input <- getContents
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
