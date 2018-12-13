import System.IO (isEOF)
import System.Random

data Point = Point {x :: Int, y :: Int, vx :: Int, vy :: Int} deriving (Show)
type Message = [Point]

-- position=< 6, 10> velocity=<-2, -1>

genCoordsFromLine :: StdGen -> Int -> Int -> String -> Message
genCoordsFromLine _ _ _ [] = []
genCoordsFromLine g x y (c:rest) =
    let restLine = genCoordsFromLine
        (tvx,g2) = randomR ((-10), 10) g
        (tvy,g3) = randomR ((-10), 10) g2
        vx = fromIntegral $ round (tvx :: Float)
        vy = fromIntegral $ round (tvy :: Float)
        restline = genCoordsFromLine g3 (x+1) y rest
    in if c == '#'
        then (Point x y vx vy):restline else restline

stepPoint :: Int -> Point -> Point
stepPoint s (Point x y vx vy) = Point (x+((-s)*vx)) (y+((-s)*vy)) vx vy

coordToString :: Point -> String
coordToString (Point x y vx vy) =
    "position=< " ++ (show x) ++ ", " ++ (show y) ++ "> velocity=<" ++ (show vx) ++ ", " ++ (show vy) ++ ">"

coordsToString :: Int -> Message -> [String]
coordsToString x = map (coordToString . stepPoint x)

readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)

main = do
    rawTargetOutput <- readLines
    g <- newStdGen
    let coords = concat $ map (uncurry (genCoordsFromLine g 0)) $ zip [0..] rawTargetOutput
        steps = 80085
    putStr $ unlines $ coordsToString steps coords