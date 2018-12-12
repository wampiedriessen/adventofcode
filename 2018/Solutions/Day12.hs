module Solutions.Day12
( solvers
) where

import CommonHelpers
import qualified Data.Map as M
import qualified Data.Sequence as S

solvers = [solveP1,solveP2]

type Pot = (Int,Bool)
type Rules = M.Map [Bool] Bool

readPot :: Char -> Bool
readPot x = if x == '#' then True else False

readPlants :: String -> [Bool]
readPlants = map readPot

readStartConfig :: String -> [Pot]
readStartConfig = zip [0,1..] . readPlants . (!! 2) . words

readRule :: String -> Rules
readRule x =
    let w = words x
    in M.insert (readPlants $ w !! 0) (readPot $ head $ w !! 2) M.empty

hasPlant :: Pot -> Bool
hasPlant = snd

potNum :: Pot -> Int
potNum = fst

expand :: [Pot] -> [Pot]
expand x = let
    first = head x
    fN = potNum first
    final = last x
    lN = potNum final
    leftExtra = map (\x -> (fN+x,False)) [(-5)..(-1)]
    rightExtra = map (\x -> (lN+x,False)) [1..5]
    in leftExtra ++ x ++ rightExtra

parseNewPlant :: Int -> (Maybe Bool) -> Pot
parseNewPlant n p =
    case p of
        Just z -> (n,z) :: Pot
        Nothing -> (n,False) :: Pot

goNewIt rules five left right = let
    [y,x,a,b,c] = five
    newPlant = parseNewPlant (potNum a) $ M.lookup (map hasPlant five) rules
    in (newPlant:newIt rules (a:x:y:left) (b:c:right))

newIt :: Rules -> [Pot] -> [Pot] -> [Pot]
newIt rules [] (a:b:c:rights) =
    let five = [(0,False),(0,False),a,b,c]
    in goNewIt rules five [] rights

newIt rules (x:y:lefts) (a:b:c:rights) =
    let five = [y,x,a,b,c]
    in goNewIt rules five lefts rights

newIt rules (x:y:lefts) (a:b:[]) =
    let five = map hasPlant [y,x,a,b,(0,False)]
        newPlant = parseNewPlant (potNum a) $ M.lookup five rules
    in (newPlant:newIt rules (a:x:y:lefts) [b])

newIt rules (x:y:lefts) [a] =
    let f = potNum a
        five = map hasPlant [y,x,a,(0,False),(0,False)]
        five2 = map hasPlant [x,a,(0,False),(0,False),(0,False)]
        five3 = map hasPlant [a,(0,False),(0,False),(0,False),(0,False)]
        newPlant = parseNewPlant f $ M.lookup five rules
        newPlant2 = parseNewPlant (f+1) $ M.lookup five2 rules
        newPlant3 = parseNewPlant (f+2) $ M.lookup five3 rules
    in [newPlant,newPlant2,newPlant3]

trimEndPots :: [Pot] -> [Pot]
trimEndPots = reverse . trim . reverse . trim
    where trim = dropWhile ((/=True) . hasPlant)

iterations :: Rules -> [Pot] -> Int -> [Pot]
iterations rules currentPlants n =
    if n == 0 then currentPlants
        else let
            expandedPots = expand $ trimEndPots currentPlants
            newPlants = newIt rules [] expandedPots
        in iterations rules newPlants (n-1)

plantScore :: [Pot] -> Int
plantScore = sum . map (\(n,p) -> if p == True then n else 0)

printPlants :: [Pot] -> String
printPlants = map (\(_,x) -> if x == True then '#' else '.')

solveP1 :: [String] -> String
solveP1 x =
    let init = readStartConfig $ head x
        rules = M.unions $ map readRule $ drop 2 x
    in show $ plantScore $ iterations rules init 20

-- || Start Part 2

findInterval rules delta score pots toGo confirmations =
    if confirmations == 0 then (pots,delta,toGo)
        else let
        newPots = iterations rules pots 1
        newScore = plantScore newPots
        newDelta = newScore - score
        in if newDelta == delta then
            findInterval rules newDelta newScore newPots (toGo-1) (confirmations-1)
        else findInterval rules newDelta newScore newPots (toGo-1) 3

solveP2 :: [String] -> String
solveP2 x =
    let init = readStartConfig $ head x
        rules = M.unions $ map readRule $ drop 2 x
        first = iterations rules init 20
        firstScore = plantScore first
        (stablePots, delta, left) = findInterval rules 0 firstScore first (50000000000-20) 3
    in show $ delta * left + (plantScore stablePots)