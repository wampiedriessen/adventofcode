module CommonHelpers
( splitInHalf
, getIntList
, countOccurs
, splitCommaSpaceDelimited
, countMostPrevalent
, unwrapInt
) where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M

-- Gets integer lists per line of input
getIntList :: String -> [[Int]]
getIntList = map (map C.digitToInt) . lines

-- Split a list in half lengthwise

splitInHalf :: [a] -> ([a], [a])
splitInHalf l = splitAt (((length l) + 1) `div` 2) l

-- folds the list to a list of tuples with all the unique values and their occurance count

countOccurs :: (Ord a) => [a] -> [(a, Int)]
countOccurs = map (\x -> (head x, length x)) . L.group . L.sort

-- like 'words', but including the comma that separates
splitCommaSpaceDelimited :: String -> [String]
splitCommaSpaceDelimited = map (takeWhile (/=',')) . words

countMostPrevalent :: (Ord a) => [a] -> Int
countMostPrevalent [] = 0
countMostPrevalent (x:xs) = maximum $ M.elems $ foldr foldFun (M.fromList [(x,1)]) xs
    where foldFun x acc = if M.member x acc then M.insertWith (+) x 1 acc else M.insert x 1 acc

unwrapInt :: Maybe Int -> Int
unwrapInt x = case x of
        Just x -> x
        Nothing -> -1

-- Default Binary Tree datastructure
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

treeSingleton :: a -> Tree a
treeSingleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = treeSingleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

-- Default Heap datastructure UNFINISHED - Copy from Tree
-- data Heap a = EmptyHeap | Node a [Heap] deriving (Show, Read, Eq)

-- heapSingleton :: a -> Heap a
-- heapSingleton x = Node x []

-- heapInsert :: (Ord a) => a -> Heap a -> Heap a
-- heapInsert x EmptyHeap = heapSingleton x
-- heapInsert x (Node a left right)
--     | x == a = Node x left right
--     | x < a  = Node a (heapInsert x left) right
--     | x > a  = Node a left (heapInsert x right)