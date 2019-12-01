module Solutions.Day16
( solvers
, listOperatorMapping
) where

import CommonHelpers
import ElfCode
import Data.Bits
import Data.List
import qualified Data.List.Split
import qualified Data.IntMap.Strict as IM

solvers = [solveP1,solveP2]

evalFakes :: Instruction -> Registers -> Registers
evalFakes (Op x a b c) regs = let
    ra = getRegVal regs a
    rb = getRegVal regs b
    in IM.insert c (case x of
    0  -> ra + rb
    1  -> ra + b
    2  -> ra * rb
    3  -> ra * b
    4  -> ra .&. rb
    5  -> ra .&. b
    6  -> ra .|. rb
    7  -> ra .|. b
    8  -> ra
    9  -> a
    10 -> if a > rb then 1 else 0
    11 -> if ra > b then 1 else 0
    12 -> if ra > rb then 1 else 0
    13 -> if a == rb then 1 else 0
    14 -> if ra == b then 1 else 0
    15 -> if ra == rb then 1 else 0
    16 -> error "unknown opcode!") regs

parseExampleReg :: String -> Registers
parseExampleReg x = IM.fromList $ zip [0..] ((read $ drop 8 x) :: [Int])

parseInput :: [String] -> ([[String]],[String])
parseInput x = let
    examples = map (\[a,b,c,_] -> [a,b,c]) $ takeWhile ((/=0) . length . head) $ Data.List.Split.chunksOf 4 x
    lengthPart1 = 4 * (length examples) + 2
    restInputs = drop lengthPart1 x
    in (examples, restInputs)

mogelijkeOpCodes :: Registers -> Registers -> [Int] -> [Int]
mogelijkeOpCodes regBefore targetReg [a,b,c] = map fst $ filter ((==targetReg) . snd) $ map (\x -> (x,evalFakes (Op x a b c) regBefore)) [0..15]

getMogelijkeOpCodeMapping :: [String] -> (Int,[Int])
getMogelijkeOpCodeMapping [before, inst, after] = let
    regBefore = parseExampleReg before
    [o, a, b, c] = map read $ words inst
    targetReg = parseExampleReg after
    in (o, mogelijkeOpCodes regBefore targetReg [a,b,c])

meerDanDrieKunnenHetZijn :: [String] -> Bool
meerDanDrieKunnenHetZijn = (3 <=) . length . snd . getMogelijkeOpCodeMapping

solveP1 :: [String] -> String
solveP1 x = let
    (examples,_) = parseInput x
    in show $ length $ filter meerDanDrieKunnenHetZijn examples

-- || Start Part 2

foldMapping :: [(Int,[Int])] -> IM.IntMap Int
foldMapping [(a,[b])] = IM.singleton a b
foldMapping m = let
    (real,[fake]) = head $ filter ((==1) . length . snd) m
    todo = map (\(r, fs) -> (r,filter (/=fake) fs)) $ delete (real,[fake]) m
    in IM.insert real fake $ foldMapping todo

getOperatorMapping :: [[String]] -> IM.IntMap Int
getOperatorMapping examples = let
    mappingRealToPossibleFakes = IM.fromListWith Data.List.intersect $ map getMogelijkeOpCodeMapping examples
    in foldMapping $ IM.toList mappingRealToPossibleFakes

solveP2 :: [String] -> String
solveP2 x = let
    (examples,instructions) = parseInput x
    real2fake = getOperatorMapping examples
    realInstructions = map ((\[r,a,b,c] -> evalFakes (Op (unwrapInt $ IM.lookup r real2fake) a b c)) . map read . words) instructions
    program = foldl1 (flip (.)) realInstructions
    in show $ unwrapInt $ IM.lookup 0 $ program IM.empty

-- || Extra

listOperatorMapping = show . getOperatorMapping . fst . parseInput