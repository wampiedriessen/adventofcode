module Solutions.Day18
( solvers
) where

import CommonHelpers
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Hashable as H

data Acre = Empty | Lumber | Trees deriving (Eq,Ord)
type State = M.Map Coord Acre

solvers = [solveP1,solveP2]

toChar x = case x of
        Empty -> "."
        Lumber -> "#"
        Trees -> "|"

printState :: State -> String
printState state = let
    w = 49
    coords = [[Point y x | x <- [0..w]] | y <- [0..w]]
    in concatMap (\x -> "\n" ++ concatMap (toChar . (state M.!)) x) coords

getNeighbours :: Coord -> [Coord]
getNeighbours p = let
    moves = [leftOf . above, above, rightOf . above, leftOf, rightOf, leftOf . below, below, rightOf . below]
    in map (\x -> (x p)) moves

outOfBounds :: Coord -> Bool
outOfBounds (Point y x) = (y<0) || (x<0) || (y>=50) || (x>=50)

boundedNeigbourhood :: Coord -> [Coord]
boundedNeigbourhood = filter (not . outOfBounds) . getNeighbours

toAcre x = case x of
    '.' -> Empty
    '#' -> Lumber
    '|' -> Trees
    otherwise -> error "unknown character in input"

countNeighbourStates state = map (state M.!) . boundedNeigbourhood
threeOrMoreX x state = (>=3) . length . filter (==x) . countNeighbourStates state
threeOrMoreTrees = threeOrMoreX Trees
threeOrMoreYards = threeOrMoreX Lumber

parseInput :: [String] -> State
parseInput str = let
    w = (length $ head str) - 1
    all = concat str
    acres = map toAcre all
    coords = [Point y x | y <- [0..w], x <- [0..w]]
    in M.fromList $ zip coords acres

evolveAcre states coord curstate = case curstate of
    Empty -> if threeOrMoreTrees states coord then Trees else Empty
    Trees -> if threeOrMoreYards states coord then Lumber else Trees
    Lumber -> let
        neighbourStates = countNeighbourStates states coord
        atLeastOneYard = (>=1) . length . filter (==Lumber)
        atLeastOneTree = (>=1) . length . filter (==Trees)
        in if (atLeastOneYard neighbourStates) && (atLeastOneTree neighbourStates)
            then Lumber
            else Empty

evolve state = M.mapWithKey (evolveAcre state) state

resourceValue elements = let
    (lumbers,rest) = L.partition (==Lumber) elements
    trees = filter (==Trees) rest
    in (length lumbers) * (length trees)

rValueFor num startState = let
    evolutions = iterate evolve startState
    in resourceValue $ M.elems $ (evolutions !! num)

solveP1 :: [String] -> String
solveP1 = show . rValueFor 10 . parseInput


-- || Start Part 2

type StateMap = M.Map [Acre] Int
type Invariant = (Bool, StateMap, State, Int)

findFirstCollision :: Invariant -> Invariant
findFirstCollision (_,prevStates,state, n) = let
    els = M.elems state
    in case M.lookup els prevStates of
        Nothing -> (False, M.insert els n prevStates, evolve state, n+1)
        Just _ -> (True, prevStates, state, n)

solveP2 :: [String] -> String
solveP2 input = let
    state = parseInput input
    collisionList = iterate findFirstCollision (False, M.empty, state, 0)
    (_,pStates,firstCollision,endOfLoop) = head $ dropWhile (\(x,_,_,_) -> x == False) collisionList
    startOfLoop = pStates M.! M.elems firstCollision
    loopModMinutes = mod (1000000000 - startOfLoop) (endOfLoop - startOfLoop)
    in show $ rValueFor (startOfLoop+loopModMinutes) state