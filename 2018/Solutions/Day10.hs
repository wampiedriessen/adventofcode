module Solutions.Day10
( solvers
) where

import CommonHelpers
import Data.List

solvers = [solveP1,solveP2]

type Message = [Point]
data Point = Point {x :: Int, y :: Int, vx :: Int, vy :: Int}

-- position=< 6, 10> velocity=<-2, -1>
parsePoints :: [String] -> Message
parsePoints = map (\x -> Point {x = readX x, y = readY x, vx = readVx x, vy = readVy x})
    where
        readX = read . takeWhile (/=',') . tail . dropWhile (/='<')
        readY = read . takeWhile (/='>') . tail . dropWhile (/=',')
        readVx = read . takeWhile (/=',') . tail . dropWhile (/='<') . dropWhile (/='v')
        readVy = read . takeWhile (/='>') . tail . dropWhile (/=',') . dropWhile (/='v')

step :: Int -> Point -> Point
step stepsize (Point x y vx vy) =
    let newX = x+(stepsize * vx)
        newY = y+(stepsize * vy)
    in Point {x = newX, y = newY, vx = vx, vy = vy}

toTuple :: Point -> (Int,Int)
toTuple (Point x y _ _) = (x,y)

stepM :: Int -> Message -> Message
stepM stepsize = map (step stepsize)

messageSize :: Message -> Int
messageSize msg =
    let
        m = toTupleM msg
        xs = map fst m
        ys = map snd m
    in ((maximum xs)-(minimum xs)) * ((maximum ys)-(minimum ys))

toTupleM :: Message -> [(Int,Int)]
toTupleM = map toTuple

findTightest :: Int -> Message -> (Int,Message)
findTightest d points =
    let newPs = stepM 1 points
        newDs = messageSize newPs
    in if newDs > d
        then (0,points)
        else
            let new = findTightest newDs newPs
            in (1+(fst new),snd new)

printableMessage :: Message -> [String]
printableMessage msg =
    let
        m = sort $ toTupleM msg
        xs = map fst m
        ys = map snd m
        getChar p = if p `elem` m then '#' else ' '
    in [[getChar (x,y) | x <- [(minimum xs)..(maximum xs)]] | y <- [(minimum ys)..(maximum ys)]]

printMessage :: Message -> String
printMessage = ('\n':) . unlines . printableMessage

solveP1 :: [String] -> String
solveP1 x =
    let
        basePoints = stepM 1 $ parsePoints x
        tightest = findTightest (messageSize basePoints) basePoints
    in printMessage $ snd tightest

-- || Start Part 2

solveP2 :: [String] -> String
solveP2 x =
    let
        basePoints = stepM 1 $ parsePoints x
        tightest = findTightest (messageSize basePoints) basePoints
    in show $ 1 + (fst tightest)