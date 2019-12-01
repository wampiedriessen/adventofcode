module Solutions.Day13
( solvers
) where

import CommonHelpers
import qualified Data.List as L
import Data.Function (on)

data CartDir = HeadingLeft | HeadingStraight | HeadingRight deriving (Eq, Show)
data Cart = CartLeft CartDir | CartUp CartDir | CartDown CartDir | CartRight CartDir | NoCart deriving (Eq, Show)
data GridItem = BotRight | TopRight | UpDown | LeftRight | Crossing | Nada deriving (Eq, Show)
type GridCoord = (Int,Int)

solvers = [solveP1,solveP2]

readGridItem :: Char -> (GridItem,Cart)
readGridItem c = case c of
        '|' -> (UpDown, NoCart)
        '-' -> (LeftRight, NoCart)
        '/' -> (BotRight, NoCart)
        '\\' -> (TopRight, NoCart)
        '+' -> (Crossing, NoCart)
        '>' -> (LeftRight, CartRight HeadingLeft)
        '<' -> (LeftRight, CartLeft HeadingLeft)
        '^' -> (UpDown, CartUp HeadingLeft)
        'v' -> (UpDown, CartDown HeadingLeft)
        otherwise -> (Nada, NoCart)

readGridLine :: String -> [(GridItem,Cart)]
readGridLine = map readGridItem

readGrid :: [String] -> [[(GridItem,Cart)]]
readGrid = map readGridLine

parseInput :: [String] -> ([[GridItem]],[(GridCoord,Cart)])
parseInput x =
    let input = readGrid x
        width = length $ head input
        height = length input
        isCartInput x y = (/= NoCart) $ snd $ ((input !! y) !! x)
        cartCoords = filter (uncurry isCartInput) $ [(x,y) | x <- [0..(width-1)], y <- [0..(height-1)]]
        carts = map (\(x,y) -> ((x,y),snd ((input !! y) !! x))) cartCoords
        tracks = map (map fst) input
    in (tracks,carts)

turnCart :: GridItem -> Cart -> Cart
turnCart x c = case x of
    BotRight -> case c of
        CartLeft d -> CartDown d
        CartRight d -> CartUp d
        CartUp d -> CartRight d
        CartDown d -> CartLeft d
    TopRight -> case c of
        CartLeft d -> CartUp d
        CartRight d -> CartDown d
        CartUp d -> CartLeft d
        CartDown d -> CartRight d

cartCrossing :: Cart -> Cart
cartCrossing c = case c of
    CartLeft HeadingLeft -> CartDown HeadingStraight
    CartLeft HeadingStraight -> CartLeft HeadingRight
    CartLeft HeadingRight -> CartUp HeadingLeft
    CartRight HeadingLeft -> CartUp HeadingStraight
    CartRight HeadingStraight -> CartRight HeadingRight
    CartRight HeadingRight -> CartDown HeadingLeft
    CartUp HeadingLeft -> CartLeft HeadingStraight
    CartUp HeadingStraight -> CartUp HeadingRight
    CartUp HeadingRight -> CartRight HeadingLeft
    CartDown HeadingLeft -> CartRight HeadingStraight
    CartDown HeadingStraight -> CartDown HeadingRight
    CartDown HeadingRight -> CartLeft HeadingLeft

solveCartCrossing :: GridItem -> Cart -> Cart
solveCartCrossing x c = case x of
    BotRight -> turnCart x c
    TopRight -> turnCart x c
    Crossing -> cartCrossing c
    otherwise -> c

nextCartLoc :: (GridCoord,Cart) -> GridCoord
nextCartLoc ((x,y),cart) =
    case cart of
        CartLeft _ -> (x-1,y)
        CartRight _ -> (x+1,y)
        CartUp _ -> (x,y-1)
        CartDown _ -> (x,y+1)

stepcart :: [[GridItem]] -> (GridCoord,Cart) -> (GridCoord,Cart)
stepcart tracks cart =
    let (x,y) = nextCartLoc cart
        trackItem = ((tracks !! y) !! x)
    in ((x,y),solveCartCrossing trackItem $ snd cart)

hasCollision :: GridCoord -> [GridCoord] -> Bool
hasCollision cart = any (==cart)

flipCoord :: GridCoord -> GridCoord
flipCoord (x,y) = (y,x)

sortCarts :: [(GridCoord,Cart)] -> [(GridCoord,Cart)]
sortCarts = L.sortBy (compare `on` (flipCoord . fst))

findFirstCollision :: [[GridItem]] -> [(GridCoord,Cart)] -> [(GridCoord,Cart)] -> GridCoord
findFirstCollision tracks [] rest = findFirstCollision tracks (sortCarts rest) []
findFirstCollision tracks (cart:carts) rest =
    let newcart = stepcart tracks cart
        coll = hasCollision (fst newcart) $ (map fst carts) ++ (map fst rest)
    in if coll == True then fst newcart
        else findFirstCollision tracks carts (newcart:rest) :: GridCoord

solveP1 :: [String] -> String
solveP1 x =
    let (tracks,carts) = parseInput x
    in show $ findFirstCollision tracks carts []

-- || Start Part 2

findLastCart :: [[GridItem]] -> [(GridCoord,Cart)] -> [(GridCoord,Cart)] -> GridCoord
findLastCart tracks [] [] = error "even amount of carts"
findLastCart tracks [x] [] = fst $ stepcart tracks x
findLastCart tracks [] rest = findLastCart tracks (sortCarts rest) []
findLastCart tracks (cart:carts) rest =
    let newcart = stepcart tracks cart
        coll = hasCollision (fst newcart) $ (map fst carts) ++ (map fst rest)
    in if coll == True
        then let
            colCoord = fst newcart
            newRest = filter ((/=colCoord) . fst) rest
            newCarts = filter ((/=colCoord) . fst) carts
        in findLastCart tracks newCarts newRest
        else findLastCart tracks carts (newcart:rest)

solveP2 :: [String] -> String
solveP2 x =
    let (tracks,carts) = parseInput x
    in show $ findLastCart tracks carts []