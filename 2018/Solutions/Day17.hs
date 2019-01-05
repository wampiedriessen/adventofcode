module Solutions.Day17
( solvers
) where

import CommonHelpers
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

solvers = [solveP1,solveP2]

data GroundState = Clay | StillWater | FallingWater deriving Eq
data Coord = Point Int Int deriving (Eq,Ord,Show)
type State = M.Map Coord GroundState

getY (Point y x) = y
getX (Point y x) = x
above (Point y x) = Point (y-1) x
below (Point y x) = Point (y+1) x
leftOf (Point y x) = Point y (x-1)
rightOf (Point y x) = Point y (x+1)

splitDots :: String -> [String]
splitDots [] = []
splitDots ('.':xs) = splitDots xs
splitDots xs = (takeWhile (/='.') xs):(splitDots $ dropWhile (/='.') xs)

parseLine :: String -> [Coord]
parseLine input = let
    dim = head input
    vast = read $ takeWhile (/=',') $ drop 2 input
    rest = splitDots $ tail $ dropWhile (/='=') $ drop 2 input -- rest = "a..b"
    range = [(read $ head rest)..(read $ last rest)]
    in if dim == 'y' then
        map (\x -> Point vast x) range
    else
        map (\x -> Point x vast) range

parseInput :: [String] -> State
parseInput = foldl (\acc x -> M.insert x Clay acc) M.empty . concatMap parseLine

-- voidChar = ' '

-- getSoilChar :: State -> Coord -> Char
-- getSoilChar state p = case M.lookup p state of
--     Nothing -> voidChar
--     Just Clay -> '#'
--     Just StillWater -> '~'
--     Just FallingWater -> '|'

-- printGridLine :: [Int] -> State -> Int ->  String
-- printGridLine xrange state y = let
--     in foldl (\acc x -> (getSoilChar state x):acc) "" [Point y x | x <- xrange]

-- printGrid :: [Int] -> State -> String
-- printGrid xrange state = let
--     layers = map (printGridLine xrange state) [1..]
--     in unlines $ take 2000 layers

-- printResult :: Result -> String
-- printResult (_,_,_,state,_) = let
--     xs = map getX $ M.keys state
--     (lowestX,highestX) = (L.minimum xs, L.minimum xs)
--     in "\n" ++ printGrid [lowestX..highestX] state

fillHorizontal :: S.Set Coord -> State -> Coord -> (S.Set Coord, [Coord])
fillHorizontal clay state p = let
    isLeftCorner x = (x `S.member` clay) || (not $ (below x `M.member` state))
    isRightCorner x = (x `S.member` clay) || (not $ (below x `M.member` state))
    lefties = iterate leftOf (leftOf p)
    righties = iterate rightOf (rightOf p)
    toTheLeft = S.fromList $ takeWhile (not . isLeftCorner) $ lefties
    toTheRight = S.fromList $ takeWhile (not . isRightCorner) $ righties
    fallers = filter (not . (`M.member` state) . below) [lefties !! (S.size toTheLeft), righties !! (S.size toTheRight)]
    newSeen = S.insert p $ S.union toTheLeft toTheRight
    in (newSeen, fallers)

type Result = (Bool, Int, S.Set Coord, State, [Coord])

fillStep :: Result -> Result
fillStep (_, _, clay, state, []) = (True, 0, S.empty, state, [])
fillStep (_, mY, clay, state, todo) = let
    (sidewaysDrops, rest) = L.partition ((`M.member` state) . below) todo
    in if not $ L.null sidewaysDrops
        then let
            target = head sidewaysDrops
            newTodo = rest ++ tail sidewaysDrops
            (newSeen, newFallers) = fillHorizontal clay state target
            in if L.null newFallers
                then let
                    newState = foldl (\acc x -> M.insert x StillWater acc) state newSeen
                    in (False, mY, clay, newState, (above target):newTodo)
                else let
                    newState = foldl (\acc x -> M.insert x FallingWater acc) state newSeen
                    in (False, mY, clay, newState, L.nub $ newTodo ++ newFallers)
        else let -- all of todo is falling
            highestY = getY $ L.minimum todo
            (realTodo,others) = L.partition ((==highestY) . getY) todo
            newState = foldl (\acc x -> M.insert x FallingWater acc) state realTodo
            newTodo = filter ((<=mY) . getY) $ map below realTodo
            in (False, mY, clay, newState, newTodo ++ others)

getSimulationResult :: State -> Result
getSimulationResult state = let
    tap = Point 0 500
    lowestY = getY $ S.findMax $ M.keysSet state
    clay = M.keysSet $ M.filter (==Clay) state
    steps = iterate fillStep (False, lowestY, clay, state, [(below tap)])
    in head $ filter (\(x,_,_,_,_) -> x == True) steps

solveP1 :: [String] -> String
solveP1 input = let
    state = parseInput input
    highestY = getY $ S.findMin $ M.keysSet state
    (_,_,_,endstate,_) = getSimulationResult state
    water = M.filterWithKey (\k v -> v/=Clay && getY k >= highestY) endstate
    in show $ M.size water

-- || Start Part 2

solveP2 :: [String] -> String
solveP2 input = let
    state = parseInput input
    (_,_,_,endstate,_) = getSimulationResult state
    still = M.filter (==StillWater) endstate
    in show $ M.size still
