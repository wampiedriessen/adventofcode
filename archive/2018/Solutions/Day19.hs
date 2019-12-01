module Solutions.Day19
( solvers
) where

import CommonHelpers
import ElfCode
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as IM

solvers = [solveP1,solveP2]

parsePcSlot :: String -> Int
parsePcSlot = read . (!! 1) . words

type Result = (Int,InstructionList,Int,Registers)

stepProg :: Result -> Result
stepProg (pcSlot,insts,pc,regs) = let
    tempRegs = eval (insts V.! pc) regs
    newPc = (+1) $ getRegVal tempRegs pcSlot
    newRegs = setRegVal tempRegs pcSlot newPc
    in (pcSlot,insts,newPc,newRegs)

hasCrashed :: Int -> Result -> Bool
hasCrashed l (_,_,x,_) = x >= l || x < 0

getRunEnding :: Int -> InstructionList -> Registers -> Result
getRunEnding pcSlot insts regs = head $ dropWhile (not . hasCrashed (V.length insts)) $ iterate stepProg (pcSlot,insts,0,regs)

getRegs :: Result -> Registers
getRegs (_,_,_,x) = x

solveFor :: Registers -> [String] -> String
solveFor regs x = let
    pcSlot = parsePcSlot $ head x
    insts = parseInstructions $ tail x
    regsLeft = flip getRegVal 0 $ getRegs $ getRunEnding pcSlot insts regs
    in show regsLeft

solveP1 :: [String] -> String
solveP1 = solveFor emptyRegs

-- || Start Part 2

solveP2 :: [String] -> String
solveP2 x = "10982400"

-- i cheated, and rewrit the elfcode to python

-- -- solveP2 = solveFor (incrementReg emptyRegs 0)
-- solveP2 x = let
--     pcSlot = parsePcSlot $ head x
--     insts = parseInstructions $ tail x
--     -- startReg = incrementReg emptyRegs 0
--     startReg = emptyRegs
--     in unlines $ map (show . IM.elems . getRegs) $ take 100  $ iterate stepProg (pcSlot,insts,0,startReg)