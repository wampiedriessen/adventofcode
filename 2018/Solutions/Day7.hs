module Solutions.Day7
( solveP1
, solveP2
) where

import CommonHelpers
import Data.List

-- import qualified Data.Map

parseChildNodes :: String -> [String]
-- parseChildNodes [] = [""]
parseChildNodes = filter (/=">") . map (takeWhile (/=',')) . words

type Weight = Int
type Name = String
type NodeInfo = (Name, (Int, [Name]))

getWeight :: NodeInfo -> Weight
getWeight (_,(w,_)) = w

getSubnames :: NodeInfo -> [Name]
getSubnames (_,(_,s)) = s

getName :: NodeInfo -> Name
getName (n,_) = n

readLine :: String -> NodeInfo
readLine x = (takeWhile (/=' ') x, (read $ takeWhile (/=')') $ tail $ dropWhile (/='(') x, parseChildNodes $ dropWhile (/='>') x))

solveP1 :: [String] -> String
solveP1 nodes =
    let parsedNodes = map readLine nodes
        nodeNames = [x | (x,_) <- parsedNodes]
        childNodeNames = concat [x | (_,(_,x)) <- parsedNodes]
    in head $ nodeNames \\ childNodeNames

-- || PART 2

lookupNodes :: [NodeInfo] -> [Name] -> [NodeInfo]
lookupNodes _ [] = []
lookupNodes [] _ = []
lookupNodes nodes names = filter (\(n,_) -> n `elem` names) nodes

getChildren :: [NodeInfo] -> NodeInfo -> [NodeInfo]
getChildren nodesDict node = lookupNodes nodesDict (getSubnames node)

getTotalWeight :: [NodeInfo] -> NodeInfo -> Weight
getTotalWeight nodesDict curNode = (getWeight curNode) + (sum $ map (getTotalWeight nodesDict) (getChildren nodesDict curNode))

getParentNode :: [NodeInfo] -> NodeInfo -> Maybe NodeInfo
getParentNode nodesDict node =
    let foundNode = filter (\x -> (getName node) `elem` (getSubnames x)) nodesDict
    in if foundNode == []
        then Nothing
        else Just (head foundNode)

getSiblings :: [NodeInfo] -> NodeInfo -> [NodeInfo]
getSiblings nodesDict node =
    case (getParentNode nodesDict node) of
        Nothing -> [node]
        Just x -> getChildren nodesDict x


findFalseNode :: [NodeInfo] -> [NodeInfo] -> Maybe NodeInfo
findFalseNode nodesDict nodes =
    let falseWeight = filter (\x -> length x == 1) $ group $ sort $ map (getTotalWeight nodesDict) nodes
        in if falseWeight == []
            then Nothing
            else Just $ head $ filter (\x -> (getTotalWeight nodesDict x) == (head $ head falseWeight)) nodes

recFindFalse :: [NodeInfo] -> NodeInfo -> NodeInfo
recFindFalse nodesDict curNode =
    let foundNode = findFalseNode nodesDict (getChildren nodesDict curNode)
    in case foundNode of
        Nothing -> curNode
        Just x -> recFindFalse nodesDict x

solveP2 :: [String] -> String
solveP2 nodes =
    let nodesDict = map readLine nodes
        rootnode = head $ lookupNodes nodesDict [(solveP1 nodes)]
        tooHeavyNode = recFindFalse nodesDict rootnode
        siblings = getSiblings nodesDict tooHeavyNode
        outstr = foldl (\acc x -> (acc ++ "> " ++ getName x ++ ": totalWeight: " ++ (show (getTotalWeight nodesDict x)) ++ ", weight: " ++ (show (getWeight x)) ++ "\n")) "" siblings
        diff = (getTotalWeight nodesDict (head $ filter (/=tooHeavyNode) siblings)) - (getTotalWeight nodesDict tooHeavyNode)
    in show $ (getWeight tooHeavyNode) + diff
    -- in outstr


