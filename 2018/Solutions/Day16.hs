module Solutions.Day16
( solvers
) where

import CommonHelpers
import Data.Bits
import Data.List
import qualified Data.List.Split
import qualified Data.IntMap.Strict as IM

type Registers = IM.IntMap Int
data Instruction = Op Int Int Int Int

solvers = [solveP1,solveP2]

addr_instruction = 0  -- (add register) stores into register C the result of adding register A and register B.
addi_instruction = 1  -- (add immediate) stores into register C the result of adding register A and value B.
mulr_instruction = 2  -- (multiply register) stores into register C the result of multiplying register A and register B.
muli_instruction = 3  -- (multiply immediate) stores into register C the result of multiplying register A and value B.
banr_instruction = 4  -- (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
bani_instruction = 5  -- (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
borr_instruction = 6  -- (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
bori_instruction = 7  -- (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
setr_instruction = 8  -- (set register) copies the contents of register A into register C. (Input B is ignored.)
seti_instruction = 9  -- (set immediate) stores value A into register C. (Input B is ignored.)
gtir_instruction = 10 -- (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
gtri_instruction = 11 -- (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
gtrr_instruction = 12 -- (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
eqir_instruction = 13 -- (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
eqri_instruction = 14 -- (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
eqrr_instruction = 15 -- (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.

getRegVal :: Registers -> Int -> Int
getRegVal regs a = case IM.lookup a regs of
    Nothing -> 0
    Just x -> x

eval :: Instruction -> Registers -> Registers
eval (Op x a b c) regs = let
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
mogelijkeOpCodes regBefore targetReg [a,b,c] = map fst $ filter ((==targetReg) . snd) $ map (\x -> (x,eval (Op x a b c) regBefore)) [0..15]

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
    (examples,part2) = parseInput x
    in show $ length $ filter meerDanDrieKunnenHetZijn examples

-- || Start Part 2

foldMapping :: [(Int,[Int])] -> IM.IntMap Int
foldMapping [(a,[b])] = IM.singleton a b
foldMapping m = let
    (real,[fake]) = head $ filter ((==1) . length . snd) m
    todo = map (\(r, fs) -> (r,filter (/=fake) fs)) $ delete (real,[fake]) m
    in IM.insert real fake $ foldMapping todo

applyList :: [x -> x] -> x -> x
applyList [f] x = f x
applyList (f:fs) x = applyList fs (f x)

solveP2 :: [String] -> String
solveP2 x = let
    (examples,instructions) = parseInput x
    mappingRealToPossibleFakes = IM.fromListWith Data.List.intersect $ map getMogelijkeOpCodeMapping examples
    real2fake = foldMapping $ IM.toList mappingRealToPossibleFakes
    realInstructions = map ((\[r,a,b,c] -> eval (Op (unwrapInt $ IM.lookup r real2fake) a b c)) . map read . words) instructions
    in show $ unwrapInt $ IM.lookup 0 $ applyList realInstructions (IM.empty :: Registers)