module Solutions.Day01
( solvers
) where

import CommonHelpers
import qualified Data.Set as Set

solvers = [solveP1,solveP2]

sanitize :: String -> Int
sanitize x =
    let
        repl '+' = ' '
        repl  c   = c
    in read (map repl x) :: Int

solveP1 :: [String] -> String
solveP1 = show . sum . map sanitize

-- || Start Part 2

solveP2Recurse :: Ord a => Set.Set a -> [a] -> a
solveP2Recurse seen (x:freqs) =
    if (Set.member x seen)
    then
        x
    else
        solveP2Recurse (Set.insert x seen) freqs

solveP2 :: [String] -> String
solveP2 x =
    let freqs = scanl (+) 0 $ cycle $ map sanitize x
    in show $ solveP2Recurse Set.empty freqs
