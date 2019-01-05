module Solutions.Day15
( solvers
) where

-- import CommonHelpers
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Set as S
import Data.Function (on)

solvers = [solveP1,solveP2]

type Coord = (Int,Int)
data Pawn = Pawn {c :: Coord, elf :: Bool, hp :: Int, ap :: Int}
data GridItem = Elf | Goblin | Wall | Field deriving (Eq)
type Gridmap = M.Map Coord GridItem
data Pawnaction = NewHp Coord Int | Killed Coord | MoveTo Coord | Stale | NoTargets | NoAttack deriving (Show)

compareCoord :: Coord -> Coord -> Ordering
compareCoord (x1,y1) (x2,y2) = compare (y1,x1) (y2,x2)

smallestCoord :: Coord -> Coord -> Coord
smallestCoord a b = case compareCoord a b of
    LT -> a
    EQ -> a
    GT -> b

smallestCoordInList :: [Coord] -> Coord
smallestCoordInList = foldl smallestCoord (9999,9999)

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
        Elf -> "E"
        Goblin -> "G"

printGridStep _ _ [] = []
printGridStep w grid coords = ('\n':showpart) ++ printGridStep w grid (drop w coords)
    where showpart = concat $ map (show . flip getGridItem grid) $ take w coords

printGrid :: Int -> Int -> Gridmap -> String
printGrid w h grid =
    let coords = [(x,y) :: Coord | y <- [0..(h-1)], x <- [0..(w-1)]]
    in printGridStep w grid coords

coordOf :: Pawn -> Coord
coordOf (Pawn x _ _ _) = x

isElf :: Pawn -> Bool
isElf (Pawn _ x _ _) = x

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
        'E' -> Elf
        'G' -> Goblin
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

createPawn :: [String] -> Int -> Coord -> Pawn
createPawn input elfPower (x,y) = if ((input !! y) !! x) == 'E'
            then Pawn (x,y) True 200 elfPower
            else Pawn (x,y) False 200 3

movePawn :: Pawn -> Coord -> Pawn
movePawn (Pawn _ e hp ap) nc = Pawn nc e hp ap

parseInput :: [String] -> Int -> (S.Set Pawn,Gridmap)
parseInput x elfPower =
    let input = readGrid x
        width = length $ head input
        height = length input
        coords = [(x,y) :: Coord | x <- [0..(width-1)], y <- [0..(height-1)]]
        soldCoords = filter (\(x,y) -> (\x -> (x == Elf || x == Goblin)) $ ((input !! y) !! x)) coords
        flipCoords = [(x,y) :: Coord | y <- [0..(height-1)], x <- [0..(width-1)]]
        soldiers = L.sort $ map (createPawn x elfPower) soldCoords
        grid = M.fromList $ zip flipCoords $ concat input
    in (S.fromList soldiers,grid)

isField :: Gridmap -> Coord -> Bool
isField grid = (==Field) . (flip getGridItem grid)

addEdge :: Gridmap -> S.Set Coord -> S.Set Coord -> [(Int,Coord)] -> (Bool, M.Map Coord Int)
addEdge _ _ _ [] = (False, M.empty)
addEdge grid targets seen ((d,top):todo) =
    if top `S.member` targets
        then (True, M.insertWith max top d $ M.fromList $ foldl (\acc (a,b) -> if a==d then (b,a):acc else acc) [] todo)
        else let
            notseen = not . (`S.member` seen)
            ns = filter (isField grid) $ getneighbours top
            newseen = S.union seen $ S.fromList ns
            newtodo = todo ++ (map (\x -> (d+1,x)) $ filter notseen ns)
            (found,distances) = addEdge grid targets newseen newtodo
        in if found then (found, M.insertWith max top d $ distances)
            else (found, distances)

floodfill :: Gridmap -> S.Set Coord -> Coord -> (Bool,M.Map Coord Int)
floodfill grid targets c = addEdge grid targets (S.singleton c) [(0,c)]

getDist :: M.Map Coord Int -> Coord -> Int
getDist dists c = case M.lookup c dists of
    Nothing -> 9999999
    Just x -> x

recTrace :: M.Map Coord Int -> Int -> Coord -> [Coord]
recTrace dists d c = if d == 1 then [c] else concatMap (recTrace dists (d-1)) $ sides c
    where sides = filter ((<d) . getDist dists) . getneighbours

traceSteps :: M.Map Coord Int -> Coord -> [Coord]
traceSteps dists c = recTrace dists (getDist dists c) c

getPlausibleTargets :: M.Map Coord Int -> S.Set Coord -> [(Coord,Int)]
getPlausibleTargets dists = foldl filterCoords [] . S.map (\x -> (x,M.lookup x dists))
    where filterCoords acc x = case snd x of
            Nothing -> acc
            Just d -> (fst x,d):acc

move :: Pawn -> S.Set Coord -> Gridmap -> Pawnaction
move p attackSpots grid =
    let (found,distances) = floodfill grid attackSpots $ coordOf p
    in if not found then Stale
        else let
            targets = getPlausibleTargets distances attackSpots
            smallestDist = snd $ L.minimumBy (compare `on` snd) targets
            closestTarget = smallestCoordInList $ L.map (fst) $ L.filter ((== smallestDist) . snd) targets
            target = smallestCoordInList $ traceSteps distances closestTarget
        in MoveTo target

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

turn :: Pawn -> S.Set Pawn -> Gridmap -> (Pawnaction, Pawnaction)
turn p enemies grid =
    let inranges = S.fromList $ concatMap (getneighbours . coordOf) enemies
    in if L.null enemies
        then (Stale, NoTargets)
        else if (coordOf p) `S.member` inranges
            then (Stale, attack p enemies)
            else let
                reachableInranges = S.filter (isField grid) inranges
                ac = (if S.null reachableInranges then Stale else move p reachableInranges grid)
            in case ac of
                Stale -> (ac, attack p enemies)
                MoveTo c -> let np = movePawn p c
                    in (ac, attack np enemies)

updateHp :: Coord -> Int -> S.Set Pawn -> S.Set Pawn
updateHp c newhp pawns = let
        curp = L.find ((==c) . coordOf) $ S.elems pawns
        in case curp of
            Nothing -> pawns
            Just p@(Pawn c e _ a) -> S.insert (Pawn c e newhp a) $ S.delete p pawns

updateMove :: Pawn -> Pawnaction -> S.Set Pawn -> Gridmap -> (S.Set Pawn,Gridmap)
updateMove old action pawns grid = case action of
    Stale -> (pawns,grid)
    MoveTo newc -> let
        tpawns = S.insert (movePawn old newc) $ S.delete old pawns
        tgrid = M.insert newc (if isElf old then Elf else Goblin) $ M.insert (coordOf old) Field grid
        in (tpawns,tgrid)

updateKilled :: Coord -> S.Set Pawn -> S.Set Pawn
updateKilled c = S.filter ((/=c) . coordOf)

battleRound :: [Pawn] -> S.Set Pawn -> Gridmap -> (Bool,S.Set Pawn,Gridmap)
battleRound [] pawns grid = (True, pawns, grid)
battleRound (toDo:rest) pawns grid =
    let (moveresult,attackresult) = turn toDo (S.filter (areEnemies toDo) pawns) grid
    in case attackresult of
        NoTargets -> (False,pawns,grid)
        Stale -> battleRound rest pawns grid
        _ -> let (tpawns, tgrid) = updateMove toDo moveresult pawns grid
            in case attackresult of
                NoAttack -> battleRound rest tpawns tgrid
                NewHp c h -> let
                        newpawns = updateHp c h tpawns
                        newrest = S.elems $ updateHp c h $ S.fromList rest
                    in battleRound newrest newpawns tgrid
                Killed c -> let
                    newgrid = M.insert c Field tgrid
                    newpawns = updateKilled c tpawns
                    newrest = S.elems $ updateKilled c $ S.fromList rest
                    in battleRound newrest newpawns newgrid

performBattle1 :: S.Set Pawn -> Gridmap -> (Int, S.Set Pawn)
performBattle1 soldiers grid =
    let
        pawns = L.sortBy (compareCoord `on` coordOf) $ S.elems soldiers
        (roundFinished, newsoldiers, newgrid) = battleRound pawns (S.fromList pawns) grid
    in if not roundFinished then (0, newsoldiers)
        else let (r, p) = performBattle1 newsoldiers newgrid
        in (r+1, p)

battleResult :: (Int, S.Set Pawn) -> String
battleResult (a,ps) = show a ++ " * " ++ show healtphoints ++ " = " ++ show (a * healtphoints)
    where healtphoints = sum $ map (hpOf) $ S.elems ps

solveP1 :: [String] -> String
solveP1 x =
    let (soldiers, grid) = parseInput x 3
    in show $ battleResult $ performBattle1 soldiers grid

-- || Start Part 2

performBattle2 :: Int -> S.Set Pawn -> Gridmap -> (Bool, Int, S.Set Pawn)
performBattle2 numElves soldiers grid =
    let
        pawns = L.sortBy (compareCoord `on` coordOf) $ S.elems soldiers
        (roundFinished, newsoldiers, newgrid) = battleRound pawns (S.fromList pawns) grid
        newNumElves = S.size $ S.filter isElf newsoldiers
    in if newNumElves /= numElves
        then (False, 0, newsoldiers)
        else if not roundFinished then (True, 0, newsoldiers)
        else let (d, r, p) = performBattle2 numElves newsoldiers newgrid
        in (d, r+1, p)

dropFirstInTriple (_,a,b) = (a,b)

solveP2 :: [String] -> String
solveP2 x =
    let numElves = L.length $ L.filter (=='E') $ L.concat x
        battleInputs = L.map (parseInput x) [3..]
    in show $ battleResult $ dropFirstInTriple $ head $ L.filter (\(noDeaths,_,_) -> noDeaths) $ L.map (uncurry (performBattle2 numElves)) battleInputs