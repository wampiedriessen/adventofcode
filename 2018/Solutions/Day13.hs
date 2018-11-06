module Solutions.Day13
( solveP1
, solveP2
) where

import CommonHelpers

parseLine :: (Read a, Integral a) => String -> (a,a)
parseLine x = (read $ takeWhile (/=':') x, read $ dropWhile (/=' ') x)

getSeverity :: (Read a, Integral a) => (a,a) -> a
getSeverity (ind,rng) =
    if (mod ind (2*(rng-1))) == 0
        then ind*rng
        else 0

getTotalSeverity :: (Read a, Integral a) => [(a,a)] -> a
getTotalSeverity = foldl (\acc x -> acc + (getSeverity x)) 0

solveP1 :: [String] -> String
solveP1 x = show $ getTotalSeverity $ map parseLine x

getScannerFunc :: (Read a, Integral a) => (a,a) -> a -> Bool
getScannerFunc (ind, rng) = (0 ==) . flip mod (2*(rng-1)) . (ind +)

getScanners :: (Read a, Integral a) => [(a,a)] -> [a -> Bool]
getScanners = map getScannerFunc

isHit :: (Read a, Integral a) => [a -> Bool] -> a -> Bool
isHit [] del = False
isHit (scn:sncs) del = if scn del
    then True
    else isHit sncs del

solveP2 :: [String] -> String
solveP2 x =
    let scanners = map parseLine x
        sFuncs = getScanners scanners
    -- in show $ head $ takeWhile
    in show $ length $ takeWhile (True==) $ map (isHit sFuncs) [0..]