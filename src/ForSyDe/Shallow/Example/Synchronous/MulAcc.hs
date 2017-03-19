-----------------------------------------------------------------------------
-- |
-- Module      :  Multiplicative Accumulator
-- Copyright   :  Tiago Pereira Vidigal
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  tiagopvidigal@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This demo is a direct adaptation of the mulacc example of the synchronous
-- (sy) model of ForSyDe-SystemC, originally made by Hosein Attarzadeh. It
-- helps to compare the change in size of the code for the same application
-- for both languages. It's also possible to correspond implementation
-- structures of Haskell and SystemC to help learning one of those languages.
--
-- It's worth to note that the functions 'mul1', 'accum' and 'add1' could be
-- directly defined in the variables assignment at the'where' statements of
-- the 'mulacc' function without losing readability. The separation in
-- multiple functions was only implemented to reflect the SystemC's code.
--
-- = Circuit explanation
--
-- The multiplicative accumulator (mulacc) multiplies two input arguments
-- and sum it with the accumulation stored (one-cycle delay, initially zero).
-- The result of the sum is stored as the new accumulation and is used in the
-- next clock cycle.
--
-- The circuit is composed by a multiplier [*], and adder [+] and a register
-- [R], with inputs 'a' and 'b' and output 'y'.
--
--       a -----,      ,--[R]<-,
--              v      v       |
--       b --->[*]--->[+]---------> y
--
-----------------------------------------------------------------------------

module MulAcc where

import ForSyDe.Shallow

-- | 'mulacc' implements the multiplicative accumulation algorithm
mulacc :: Signal Integer -- ^ operand a
       -> Signal Integer -- ^ operand b
       -> Signal Integer -- ^ number of clock cycles
       -> Signal Integer -- ^ output value after cycles
mulacc a b ticks = zipWith3SY add mult acc ticks
  where mult = mul1 a b       -- Multiply inputs
        acc  = accum addi     -- Store accumulated (output) value
        addi = add1 mult acc  -- Sum multiplication and stored value
        add x y ctrl = x + y  -- Ignore cycles' number

-- | 'mul1' implements the multiplication
mul1 :: Signal Integer -- ^ operand a
     -> Signal Integer -- ^ operand b
     -> Signal Integer -- ^ Multiplication of operands
mul1 = zipWithSY (*)

-- | 'accum' implements the accumulator (1 cycle delay)
accum :: Signal Integer -- ^ Value to be stored
      -> Signal Integer -- ^ Input delayed
accum = delaySY 0

-- | 'add1' implements the adder
add1 :: Signal Integer -- ^ operand a
     -> Signal Integer -- ^ operand b
     -> Signal Integer -- ^ Sum of operands
add1 = zipWithSY (+)
