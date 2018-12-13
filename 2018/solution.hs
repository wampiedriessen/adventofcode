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

import System.Clock
import System.Directory
import System.Environment
import System.IO
import Text.Printf

import CommonHelpers

solutions = [D01.solvers, D02.solvers, D03.solvers, D04.solvers, D05.solvers, D06.solvers, D07.solvers, D08.solvers, D09.solvers, D10.solvers, D11.solvers, D12.solvers, D13.solvers, D14.solvers, D15.solvers, D16.solvers, D17.solvers, D18.solvers, D19.solvers, D20.solvers, D21.solvers, D22.solvers, D23.solvers, D24.solvers, D25.solvers]

padShow :: (Num a, Ord a, Show a) => a -> String
padShow x = if x > 9 then show x else "0" ++ (show x)

doFunc :: ([String] -> String) -> [String] -> IO Double
doFunc solve problem = do
    start <- getTime Monotonic
    putStrLn $ solve problem
    end <- getTime Monotonic
    return ((fromIntegral $ toNanoSecs $ diffTimeSpec start end) / (10^9))

doRestFuncs :: Int -> [([String] -> String)] -> [String] -> IO ()
doRestFuncs _ [] _ = return ()
doRestFuncs i (x:xs) problem = do
    putStr $ "Bonus func " ++ (show i) ++ ": "
    doFunc x problem
    doRestFuncs (i+1) xs problem

performCalculations :: Bool -> Int -> IO ()
performCalculations isTest pNr = do
    let iType = if isTest then "test" else "d"
        fileName = "Inputs/" ++ iType ++ (padShow pNr) ++ ".txt"

    fileExists <- doesFileExist fileName

    if not fileExists then
        putStrLn $ "Day " ++ (show pNr) ++ " not solved yet"
    else do
        putStrLn $ "Day " ++ (show pNr)

        handle <- openFile fileName ReadMode
        input <- hGetContents handle

        let (solveP1:solveP2:rest) = (solutions !! (pNr - 1))
            problem = lines input

        putStr "Deel 1: "
        time1 <- doFunc solveP1 problem
        putStr "Deel 2: "
        time2 <- doFunc solveP2 problem

        printf "Timings - 1: %0.6f sec, 2: %1.6f sec\n" (time1 :: Double) (time2 :: Double)

        do case rest of
            [] -> return ()
            otherwise -> doRestFuncs 1 rest problem
        putStr "\n"

main = do
    args <- getArgs
    let argv = length args

    if argv == 0 then do
        putStrLn "Usage: ./solution ($daynr|'all') (?'test')"
    else do
        let pNr = head args
            isTest = argv == 2

        putStr "\n"

        if pNr == "all" || pNr == "a" then do
            mapM (performCalculations isTest) [1..25]
            return ()
        else do
            performCalculations isTest $ read pNr
