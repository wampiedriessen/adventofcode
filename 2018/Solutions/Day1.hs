module Solutions.Day1
( solveP1
, solveP2
) where

import CommonHelpers
import qualified Data.Set as Set

sanitize :: String -> Int
sanitize x =
    let
        repl '+' = ' '
        repl  c   = c
    in read (map repl x) :: Int

solveP1 :: [String] -> String
solveP1 = show . sum . map sanitize

-- || Start Part 2

-- hasDuplicates [] = False
-- hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs

-- hasDuplicateFrequency :: [Int] -> Bool
-- hasDuplicateFrequency [] = False
-- hasDuplicateFrequency (x:xs) = hasDuplicates (x:xs) || hasDuplicateFrequency xs

-- solveP2Recurse :: Set.Set -> [Int] -> Int
solveP2Recurse seen (x:freqs) =
    if (Set.member x seen)
    then
        x
    else
        solveP2Recurse (Set.insert x seen) freqs

solveP2 :: [String] -> String
solveP2 x =
    let
        freqs = scanl (+) 0 $ cycle $ map sanitize x
    in show $ solveP2Recurse Set.empty freqs


    -- in show $ f !! (head $ dropWhile (not . hasDuplicates . fst . flip splitAt f) [1..])
    -- in show $ fst $ splitAt 2048 f
    -- in show f
    -- in show $ f !! ( dropWhile () [1..])
    --in show [1..]