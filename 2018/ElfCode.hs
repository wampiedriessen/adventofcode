module ElfCode
( Registers
, Instruction (Op)
, addr_instruction
, addi_instruction
, mulr_instruction
, muli_instruction
, banr_instruction
, bani_instruction
, borr_instruction
, bori_instruction
, setr_instruction
, seti_instruction
, gtir_instruction
, gtri_instruction
, gtrr_instruction
, eqir_instruction
, eqri_instruction
, eqrr_instruction
, getRegVal
, eval
) where

import Data.Bits
import qualified Data.IntMap.Strict as IM

type Registers = IM.IntMap Int
data Instruction = Op Int Int Int Int

addr_instruction = 3  -- (add register) stores into register C the result of adding register A and register B.
addi_instruction = 5  -- (add immediate) stores into register C the result of adding register A and value B.
mulr_instruction = 1  -- (multiply register) stores into register C the result of multiplying register A and register B.
muli_instruction = 9  -- (multiply immediate) stores into register C the result of multiplying register A and value B.
banr_instruction = 15  -- (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
bani_instruction = 13  -- (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
borr_instruction = 8  -- (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
bori_instruction = 7  -- (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
setr_instruction = 11  -- (set register) copies the contents of register A into register C. (Input B is ignored.)
seti_instruction = 14  -- (set immediate) stores value A into register C. (Input B is ignored.)
gtir_instruction = 10 -- (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
gtri_instruction = 6 -- (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
gtrr_instruction = 0 -- (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
eqir_instruction = 12 -- (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
eqri_instruction = 2 -- (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
eqrr_instruction = 4 -- (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.

getRegVal :: Registers -> Int -> Int
getRegVal regs a = case IM.lookup a regs of
    Nothing -> 0
    Just x -> x

eval :: Instruction -> Registers -> Registers
eval (Op x a b c) regs = let
    ra = getRegVal regs a
    rb = getRegVal regs b
    in IM.insert c (case x of
    3  -> ra + rb
    5  -> ra + b
    1  -> ra * rb
    9  -> ra * b
    15 -> ra .&. rb
    13 -> ra .&. b
    8  -> ra .|. rb
    7  -> ra .|. b
    11 -> ra
    14 -> a
    10 -> if a > rb then 1 else 0
    6  -> if ra > b then 1 else 0
    0  -> if ra > rb then 1 else 0
    12 -> if a == rb then 1 else 0
    2  -> if ra == b then 1 else 0
    4  -> if ra == rb then 1 else 0
    16 -> error "unknown opcode!") regs