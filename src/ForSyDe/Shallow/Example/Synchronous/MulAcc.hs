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
-- = Description ===========================================================
--
-- This demo is a direct adaptation of the mulacc example of the synchronous
-- (sy) model of ForSyDe-SystemC, originally made by Hosein Attarzadeh. It
-- helps to compare the change in size of the code for the same application
-- for both languages. It's also possible to correspond implementation
-- structures of Haskell and SystemC to help learning one of those languages.
--
-- It's worth to note that the separation in multiple functions in the 'Main'
-- section was only implemented to reflect the SystemC's code. A more clean
-- and "common" way of implementing this system in Haskell is presented in
-- 'mulacc_alt' function in the 'Alternative version' section.
--
-- = Circuit explanation ====================================================
--
-- The multiplicative accumulator (mulacc) multiplies two input arguments
-- and sum it with the accumulation stored (one-cycle delay, initially zero).
-- The result of the sum is stored as the new accumulation and is used in the
-- next clock cycle.
--
-- The circuit is composed by a multiplier [*], and adder [+] and a register
-- [R], with inputs 'a' and 'b' and output 'y'.
--
--                                        accum
--       a -----,              ,---addi2---[R]<--,
--              v              v                 |
--       b --->[*]---addi1--->[+]------acci----------> result
--            mul1           add1
--
-- = Running the demo =======================================================
--
-- To run a demo of the mulacc, it's only necessary to run the demo functions
-- presented at the end of this file in the 'Demo run functions' section. The
-- 'sim_mulacc' runs an example using 'mulacc' and 'sim_alt' runs the same
-- example, however using 'mulacc_alt'. The signal 'ticks' was defined to
-- determine the number of iterations and 'zipWith3SY' to limit the execution
-- to have the determined number of iterations.
--
-- The demo reflects the test generated at the 'top.hpp' file of the SystemC
-- version. A constant 'srca' is used as operand 'a' and a signal generation
-- function 'siggen' creates the signal 'srcb', which is used as operand 'b'.
-- The number of iterations, or 'ticks', is defined in by 'num_iterations' of
-- the 'sim_run' function.
--
-- To correspond with the SystemC test, the 'siggen' is a increment function
-- with initial value 1, the constant value is 3 and 'num_iterations' is set
-- as 10 by default. The output of the SystemC simulation is:
--
--       output value: 3
--       output value: 9
--       output value: 18
--       output value: 30
--       output value: 45
--       output value: 63
--       output value: 84
--       output value: 108
--       output value: 135
--       output value: 165
--
-- So the expected output of the Haskell version is:
--
--       {3,9,18,30,45,63,84,108,135,165}
--
-----------------------------------------------------------------------------

module MulAcc where

import ForSyDe.Shallow

-- Main ---------------------------------------------------------------------

-- | 'mulacc' implements the multiplicative accumulation algorithm
mulacc :: Signal Integer -- ^ operand a
       -> Signal Integer -- ^ operand b
       -> Signal Integer -- ^ output value
mulacc a b = result
    where mul1   = make_scomb2 mul_func -- Multiplier
          addi1  = mul1 a b             -- Multiplier connections

          add1   = make_scomb2 add_func -- Adder
          acci   = add1 addi1 addi2     -- Adder connections

          accum  = make_sdelay 0        -- Accumulator
          addi2  = accum acci           -- Accumulator connections

          result = acci                 -- Output of the system


-- | 'make_sdelay' is the SystemC equivalent to 'delaySY' in Haskell
make_sdelay :: a        -- ^ Initial state
            -> Signal a -- ^ Input signal
            -> Signal a -- ^ Output signal
make_sdelay = delaySY

-- | 'make_scomb2' is the SystemC equivalent to 'comb2SY' in Haskell
make_scomb2 :: (a -> b -> c) -- ^ Function to be applied
            -> Signal a      -- ^ Input signal 1
            -> Signal b      -- ^ Input signal 1
            -> Signal c      -- ^ Output signal
make_scomb2 = comb2SY

-- | 'mul_func' implements the multiplier
mul_func :: Num a => (a -> a -> a) -- ^ Multiplication function
mul_func = (*)

-- | 'add_func' implements the adder
add_func :: Num a => (a -> a -> a) -- ^ Sum function
add_func = (+)


-- Alternative version ------------------------------------------------------

-- | 'mulacc_alt' is equivalent to 'mulacc', but with a more functional look
mulacc_alt :: Signal Integer -- ^ operand a
           -> Signal Integer -- ^ operand b
           -> Signal Integer -- ^ output value
mulacc_alt a b = result
    where addi1  = comb2SY (*) a b         -- Multiplier
          acci   = comb2SY (+) addi1 addi2 -- Adder
          addi2  = delaySY 0 acci          -- Accumulator
          result = acci                    -- Output of the system


-- Demo run functions -------------------------------------------------------

-- | 'num_iterations' determine the number of iterations to be simulated
num_iterations = 10

-- | 'sim_mulacc' runs the demo
sim_mulacc :: Signal Integer -- ^ output of the circuit
sim_mulacc = sim_run $ mulacc srca srcb 
    where srca = constant1
          srcb = siggen1

-- | 'sim_alt' runs the demo with the alternative version
sim_alt :: Signal Integer -- ^ output of the circuit
sim_alt = sim_run $ mulacc_alt srca srcb
    where srca = constant1
          srcb = siggen1

-- | 'sim_run' executes a given number o iterations
sim_run :: Signal Integer -- ^ Output signal from circuit
        -> Signal Integer -- ^ Output signal limited by
sim_run values = zipWithSY iteration values ticks
    where ticks = signal [1..num_iterations]
          iteration value tick = value

-- | 'constant1' uses 'sourceSY' with a dummy signal generation function ('make_sconstant' in SystemC)
constant1 :: Signal Integer -- ^ Constant signal
constant1 = make_sconstant 3
    where make_sconstant = sourceSY $ (+) 0

-- | 'siggen1' uses 'sourceSY' ('make_ssource' in SystemC)
siggen1 :: Signal Integer -- ^ Signal generated by increment
siggen1 = make_ssource siggen 1
    where siggen       = (+) 1
          make_ssource = sourceSY
