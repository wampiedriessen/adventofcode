module Solutions.Day15
( solvers
) where

import CommonHelpers
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Set as S
import Data.Function (on)

solvers = [solveP1,solveP2]

type Coord = (Int,Int)
data Pawn = Pawn {c :: Coord, isElf :: Bool, hp :: Int, ap :: Int}
data GridItem = Player | Wall | Field deriving (Eq)
type Gridmap = M.Map Coord GridItem
data Pawnaction = NewHp Coord Int | Killed Coord | Stale | NoTargets | NoAttack

compareCoord (x1,y1) (x2,y2) = compare (y1,x1) (y2,x2)

instance Show Pawn where
    show (Pawn c e h _ ) = (if e then "E" else "G") ++ show c ++ show h

instance Eq Pawn where
    (==)  (Pawn c1 _ _ _ ) (Pawn c2 _ _ _) = c1 == c2

instance Ord Pawn where
    compare (Pawn (x1,y1) _ _ _ ) (Pawn (x2,y2) _ _ _) = compare (y1,x1) (y2,x2)

instance Show GridItem where
    show x = case x of
        Wall -> "#"
        Field -> "."
        Player -> "X"

printGridStep _ _ [] = []
printGridStep w grid coords = ('\n':showpart) ++ printGridStep w grid (drop w coords)
    where showpart = concat $ map (show . flip getGridItem grid) $ take w coords

printGrid :: Int -> Int -> Gridmap -> String
printGrid w h grid =
    let coords = [(x,y) :: Coord | y <- [0..(h-1)], x <- [0..(w-1)]]
    in printGridStep w grid coords

coordOf :: Pawn -> Coord
coordOf (Pawn x _ _ _) = x

hpOf :: Pawn -> Int
hpOf (Pawn _ _ x _) = x

apOf :: Pawn -> Int
apOf (Pawn _ _ _ x) = x

areEnemies :: Pawn -> Pawn -> Bool
areEnemies (Pawn _ a _ _) (Pawn _ b _ _) = a /= b

areNeighbours :: Pawn -> Pawn -> Bool
areNeighbours (Pawn (x1,y1) _ _ _) (Pawn (x2,y2) _ _ _) = (1 ==) $ (abs (y2-y1)) + (abs (x2-x1))

readGridItem :: Char -> GridItem
readGridItem c = case c of
        'E' -> Player
        'G' -> Player
        '.' -> Field
        _ -> Wall

readGrid :: [String] -> [[GridItem]]
readGrid = map (map readGridItem)

getGridItem :: Coord -> Gridmap -> GridItem
getGridItem c m = case M.lookup c m of
    Nothing -> error "unknown coordinate"
    Just i -> i

getneighbours :: Coord -> [Coord]
getneighbours (x,y) = [(x,y-1),(x-1,y),(x+1,y),(x,y+1)]

createPawn :: [String] -> Coord -> Pawn
createPawn input (x,y) = if ((input !! y) !! x) == 'E'
            then Pawn (x,y) True 200 3
            else Pawn (x,y) False 200 3

movePawn :: Pawn -> Coord -> Pawn
movePawn (Pawn c e hp ap) nc = Pawn nc e hp ap

parseInput :: [String] -> (S.Set Pawn,Gridmap)
parseInput x =
    let input = readGrid x
        width = length $ head input
        height = length input
        coords = [(x,y) :: Coord | x <- [0..(width-1)], y <- [0..(height-1)]]
        soldCoords = filter (\(x,y) -> (== Player) $ ((input !! y) !! x)) coords
        flipCoords = [(x,y) :: Coord | y <- [0..(height-1)], x <- [0..(width-1)]]
        soldiers = L.sort $ map (createPawn x) soldCoords
        grid = M.fromList $ zip flipCoords $ concat input
    in (S.fromList soldiers,grid)

isField :: Gridmap -> Coord -> Bool
isField grid = (==Field) . (flip getGridItem grid)

recFF :: Int -> S.Set Coord -> Gridmap -> Coord -> [(Coord,Int)]
recFF d seen grid c =
    let ns = filter (not . (`S.member` seen)) $ filter (isField grid) $ getneighbours c
    in if L.null ns then [(c,d)]
        else let
            nextstep = concat $ map (recFF (d+1) (S.union (S.fromList ns) seen) grid) ns
            in (c,d):nextstep

floodfill :: Gridmap -> Coord -> M.Map Coord Int
floodfill grid c = M.fromListWith min $ recFF 1 (S.fromList [c]) grid c

move :: Pawn -> [Coord] -> Gridmap -> Pawn
move p targets grid =
    let distances = M.unionsWith min $ L.map (floodfill grid) targets
        ns = filter (isField grid) $ getneighbours $ coordOf p
        posscoords = L.filter ((/=Nothing) . snd) $ map (\x -> (x,M.lookup x distances)) ns
    in if L.null posscoords then p
        else let
            nextcoord = head $ L.sortBy (compareCoord `on` fst) $ head $ L.groupBy ((==) `on` snd) $ L.sortBy (compare `on` snd) posscoords
        in case snd nextcoord of
            Nothing -> p
            Just _ -> movePawn p (fst nextcoord)

attack :: Pawn -> S.Set Pawn -> Pawnaction
attack p enemies =
    let ns = S.elems $ S.filter (areNeighbours p) enemies
    in if L.null ns
        then NoAttack
        else let
            battle = head $ L.sortBy (compare `on` hpOf) ns
            newHp = (hpOf battle) - (apOf p)
        in if newHp <= 0
            then Killed (coordOf battle)
            else NewHp (coordOf battle) newHp

turn :: Pawn -> S.Set Pawn -> Gridmap -> (Pawn, Pawnaction)
turn p enemies grid =
    let targets = S.map coordOf enemies
        inranges = concatMap getneighbours targets
    in if L.null targets
        then (p, NoTargets)
        else if L.null inranges
            then (p, Stale)
            else if (coordOf p) `L.elem` inranges
                then (p, attack p enemies)
                else let
                    reachableInranges = L.filter (isField grid) inranges
                    np = move p reachableInranges grid
                    in (np, attack np enemies)

updateHp :: Coord -> Int -> S.Set Pawn -> S.Set Pawn
updateHp c newhp pawns = let
        curp = L.find ((==c) . coordOf) $ S.elems pawns
        in case curp of
            Nothing -> pawns
            Just p@(Pawn c e _ a) -> S.insert (Pawn c e newhp a) $ S.delete p pawns

updateMove :: Pawn -> Pawn -> S.Set Pawn -> Gridmap -> (S.Set Pawn,Gridmap)
updateMove old new pawns grid = let
    tpawns = S.insert new $ S.delete old pawns
    tgrid = M.insert (coordOf new) Player $ M.insert (coordOf old) Field grid
    in (tpawns,tgrid)

updateKilled :: Coord -> S.Set Pawn -> S.Set Pawn
updateKilled c = S.filter ((/=c) . coordOf)

battleRound :: [Pawn] -> S.Set Pawn -> Gridmap -> (Bool,S.Set Pawn,Gridmap)
battleRound [] pawns grid = (True, pawns, grid)
battleRound (toDo:rest) pawns grid =
    let (np,updated) = turn toDo (S.filter (areEnemies toDo) pawns) grid
    in case updated of
        NoTargets -> (False,pawns,grid)
        Stale -> battleRound rest pawns grid
        NoAttack -> uncurry (battleRound rest) $ updateMove toDo np pawns grid
        NewHp c h -> let
                tpawns = updateHp c h pawns
                newrest = S.elems $ updateHp c h $ S.fromList rest
                (newpawns,newgrid) = updateMove toDo np tpawns grid
            in battleRound newrest newpawns newgrid
        Killed c -> let
            tgrid = M.insert c Field grid
            tpawns = updateKilled c pawns
            newrest = S.elems $ updateKilled c $ S.fromList rest
            (newpawns,newgrid) = updateMove toDo np tpawns tgrid
            in battleRound newrest newpawns newgrid

-- isMonogamous :: [Pawn] -> Bool
-- isMonogamous [] = True
-- isMonogamous [x] = True
-- isMonogamous (x:y:xs) = (not $ areEnemies x y) && isMonogamous (y:xs)

performBattle :: S.Set Pawn -> Gridmap -> (Int,S.Set Pawn,Gridmap)
performBattle soldiers grid =
    let
        pawns = L.sortBy (compareCoord `on` coordOf) $ S.elems soldiers
        (roundFinished, newsoldiers, newgrid) = battleRound pawns (S.fromList pawns) grid
    in if not roundFinished then (0, newsoldiers, newgrid)
        else let (r, p, g) = performBattle newsoldiers newgrid
        in (r+1,p,g)

battleResult :: (Int,S.Set Pawn,Gridmap) -> String
battleResult (a,ps,_) = show a ++ " * " ++ show healtphoints ++ " = " ++ show (a * healtphoints)
    where healtphoints = sum $ map (hpOf) $ S.elems ps

thrd (_,_,x) = x

solveP1 :: [String] -> String
solveP1 x =
    let (soldiers, grid) = parseInput x
        bround (_,x,y) = battleRound (S.elems x) x y
        f = iterate bround (True,soldiers,grid)
    in unlines $ map (printGrid 32 32) $ map (\x -> thrd (f !! x)) [0..38]
    -- in show $ battleResult $ performBattle soldiers grid
    -- in show $ performBattle soldiers grid
    --     p = last soldiers
    --     targets = findTargets p soldiers
    --     distances = M.unionsWith min $ map (floodfill grid) targets
    --     ns = filter (isField grid) $ getneighbours $ coordOf p
    --     posscoords = L.filter ((/=Nothing) . snd) $ map (\x -> (x,M.lookup x distances)) ns
    -- in show ns

-- || Start Part 2

solveP2 :: [String] -> String
solveP2 x = "ToDo: part 2"