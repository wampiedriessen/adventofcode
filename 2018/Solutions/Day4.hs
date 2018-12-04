module Solutions.Day4
( solveP1
, solveP2
) where

import CommonHelpers
import qualified Data.List as L
import qualified Data.Set as S
import Data.Function (on)

parseDate :: String -> Integer
parseDate x =
    let
        readMonth = read . takeWhile (/='-') . tail . dropWhile (/='-')
        readDay = read . takeWhile (/=' ') . tail . dropWhile (/='-') . tail . dropWhile (/='-')
        readHour = read . takeWhile (/=':') . tail . dropWhile (/=' ')
        readMinute = read . takeWhile (/=']') . tail . dropWhile (/=':')
        monthsToMinutes x = if x `elem` [1,3,5,7,8,10,12] then x*60*24*31 else x*60*24*30
    in (monthsToMinutes (readMonth x)) + (readDay x * 60 * 24) + (readHour x * 60) + (readMinute x)

isGuardLine :: String -> Bool
isGuardLine = ("Guard" `L.isInfixOf`)

isWakeLine :: String -> Bool
isWakeLine = ("wakes" `L.isInfixOf`)

isSleepLine :: String -> Bool
isSleepLine = ("falls" `L.isInfixOf`)

getGuardId :: String -> String
getGuardId = (!! 3) . words

getGuardIds :: [String] -> [String]
getGuardIds = map getGuardId . filter isGuardLine

parseStates :: Integer -> String -> [String] -> [(String, Integer)]
parseStates _ __ [] = []
parseStates prevtime curGuard (newline:xs) =
    let
        newtime = parseDate newline
    in if isWakeLine newline then
        map (\x -> (curGuard,mod x 60)) [prevtime..newtime-1] ++ parseStates newtime curGuard xs
    else
        if isGuardLine newline then
            parseStates newtime (getGuardId newline) xs
        else
            parseStates newtime curGuard xs

minutesAsleep :: [(String, Integer)] -> String -> Int
minutesAsleep states guard =
    length $ filter ((== guard) . fst) states

mostLazyMinute :: [(String, Integer)] -> String -> Integer
mostLazyMinute states guard =
    let guardStates = filter ((== guard) . fst) states
        compareMost x = length $ filter ((== x) . snd) guardStates
    in last $ L.sortBy (compare `on` compareMost) [0..59]

solveP1 :: [String] -> String
solveP1 x =
    let
        guardIds = L.nub $ getGuardIds x
        firstdate = parseDate $ head $ L.sort x
        -- lastdate = parseDate $ last & L.sort x
        states = parseStates firstdate "wampie" (L.sort x)
        lazyGuard = L.maximumBy (compare `on` (minutesAsleep states)) guardIds
    -- in show $ map (parseDate . fst . splitAt 18) x
    in show $ (mostLazyMinute states lazyGuard) * (read $ tail lazyGuard)

-- || Start Part 2

solveP2 :: [String] -> String
solveP2 x =
    let
        guardIds = L.nub $ getGuardIds x
        firstdate = parseDate $ head $ L.sort x
        -- lastdate = parseDate $ last & L.sort x
        states = parseStates firstdate "wampie" (L.sort x)
        winningPair = head $ last $ L.sortBy (compare `on` length) $ L.group $ L.sort $ states
    -- in show $ map (parseDate . fst . splitAt 18) x
    in show $ (read $ tail $ fst winningPair) * (snd winningPair)