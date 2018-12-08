module Solutions.Day08
( solvers
) where

solvers = [solveP1,solveP2]

data Tree = Node [Int] [Tree] deriving (Show, Read, Eq)

parse :: [String] -> [Int]
parse = map read . words . head

accumulateChildren :: ([Tree],[Int]) -> a -> ([Tree],[Int])
accumulateChildren acc x =
    let (nd,rst) = parseTree $ snd acc
        in ((fst acc) ++ [nd],rst)

parseTree :: [Int] -> (Tree, [Int])
parseTree (0:x:rest) = (Node (take x rest) [], drop x rest)
parseTree (num:x:rest) =
    let (children,restInput) = foldl accumulateChildren ([],rest) [1..num]
    in (Node (take x restInput) children, drop x restInput)

solveP1 :: [String] -> String
solveP1 = show . recurse . fst . parseTree . parse
    where recurse (Node mData children) = (sum mData) + (sum $ map recurse children)

-- || Start Part 2

recursep2 :: Tree -> Int
recursep2 (Node mData children) =
    if 0 == (length children)
        then (sum mData)
        else sum $ map (getChildvalue children) mData
    where getChildvalue children index = if (index-1) >= length children
            then 0
            else recursep2 $ children !! (index-1)

solveP2 :: [String] -> String
solveP2 = show . recursep2 . fst . parseTree . parse