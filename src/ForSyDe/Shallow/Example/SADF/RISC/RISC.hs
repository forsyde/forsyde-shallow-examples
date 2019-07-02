-----------------------------------------------------------------------------
-- |
-- Module  :  RISC
-- Copyright   :  (c) Ricardo Bonna
-- License     :  still needs license
--
-- Maintainer  :  ricardobonna@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This is the second version of the RISC processor featuring a smaller
-- and more abstract concept. This makes easier to implement new operations.
--
-----------------------------------------------------------------------------

module ForSyDe.Shallow.Example.SADF.RISC.RISC where

import ForSyDe.Shallow
import Data.Bits((.&.), (.|.), xor)

---------------------------------------------------------
-- Instruction Fetch (IF) Kernel
---------------------------------------------------------

type ScenarioIF = ((Int, Int, Int), (Int, Int, Int, Int), [Int] -> [Int]
                -> [Vector String] -> ([[Int]], [String], [Int], [Vector String]))

-- | 'ifScenario' is the list of possible scenarios for the IF kernel
ifScenario :: Int -> ScenarioIF
ifScenario n
  | n > 20 || n < 0   = error "ifScenario: Non existent scenario"
  | n >= 14 && n < 19 = ((1,1,1), (1,1,1,1), \[a] [pc] [m] -> ([arg m (pc+a)], [op m (pc+a)], [pc+a+1], [m])) -- branch
  | otherwise         = ((0,1,1), (1,1,1,1), \_ [pc] [m] -> ([arg m pc], [op m pc], [pc+1], [m]))             -- no branch

-- | 'slice' takes a string containing one assembly command and slice it into
-- one string with the opcode and a list of Ints containing the arguments
slice :: String -> (String, [Int])
slice a = (head $ words a, args)
  where rest = tail $ words a
        args
          | null rest = []
          | length rest == 1 = [read (head rest) :: Int]
          | length rest == 2 = [read (head rest) :: Int, read (last rest) :: Int]
          | otherwise = error "slice: Some instruction has more than 2 arguments"

arg :: (Eq a, Num a, Integral a) => Vector String -> a -> [Int]
arg x y = snd $ slice (atV x y)

op :: (Eq a, Num a, Integral a) => Vector String -> a -> String
op x y = fst $ slice (atV x y)

-- | 'ifKernel' is the Instruction Fetch (IF) kernel process
ifKernel :: Vector String -> Signal ScenarioIF -> Signal Int -> (Signal [Int], Signal String)
ifKernel progV ifCt sigBr = (sigArg, sigOp)
  where (sigArg, sigOp, sigPc, sigPm) = kernel34SADF ifCt sigBr sigPc' sigPm'
        sigPc' = delaySADF [0] sigPc
        sigPm' = delaySADF [progV] sigPm


---------------------------------------------------------
-- Execute (EXE) Kernel
---------------------------------------------------------

type ScenarioEXE = ((Int, Int, Int), (Int, Int, Int, Int), [[Int]] -> [Vector Int]
                 -> [Vector Int] -> ([Int], [Int], [Vector Int], [Vector Int]))

-- | 'exeScenario' is the list of possible scenarios for the EXE kernel
exeScenario :: Int -> ScenarioEXE
exeScenario 0  = ((1,0,0), (0,0,0,0), \_ _ _ -> ([], [], [], []))                                                     -- nop
exeScenario 1  = ((1,1,1), (0,0,1,1), \[[rd, mn]] [r] [m] -> ([], [], [replaceV r rd (atV m mn)], [m]))               -- ld
exeScenario 2  = ((1,1,1), (0,0,1,1), \[[rd, rm]] [r] [m] -> ([], [], [replaceV r rd (atV m (atV r rm))], [m]))       -- ldr
exeScenario 3  = ((1,1,1), (0,0,1,1), \[[rs, mn]] [r] [m] -> ([], [], [r], [replaceV m mn (atV r rs)]))               -- st
exeScenario 4  = ((1,1,1), (0,0,1,1), \[[rs, rm]] [r] [m] -> ([], [], [r], [replaceV m (atV r rm) (atV r rs)]))       -- str
exeScenario 5  = ((1,1,0), (0,0,1,0), \[[rd, rs]] [r] _ -> ([], [], [replaceV r rd (atV r rs)], []))                  -- mov
exeScenario 6  = ((1,1,0), (0,0,1,0), \[[rd, i]] [r] _ -> ([], [], [replaceV r rd i], []))                            -- movi
exeScenario 7  = ((1,1,0), (0,0,1,0), \[[rd, rs]] [r] _ -> ([], [], [replaceV r rd ((atV r rd) + (atV r rs))], []))   -- add
exeScenario 8  = ((1,1,0), (0,0,1,0), \[[rd, rs]] [r] _ -> ([], [], [replaceV r rd ((atV r rd) - (atV r rs))], []))   -- sub
exeScenario 9  = ((1,1,0), (0,0,1,0), \[[rd, rs]] [r] _ -> ([], [], [replaceV r rd ((atV r rd) * (atV r rs))], []))   -- mul
exeScenario 10 = ((1,1,0), (0,0,1,0), \[[rd, rs]] [r] _ -> ([], [], [replaceV r rd (div (atV r rd) (atV r rs))], [])) -- div
exeScenario 11 = ((1,1,0), (0,0,1,0), \[[rd, rs]] [r] _ -> ([], [], [replaceV r rd ((atV r rd) .&. (atV r rs))], [])) -- and
exeScenario 12 = ((1,1,0), (0,0,1,0), \[[rd, rs]] [r] _ -> ([], [], [replaceV r rd ((atV r rd) .|. (atV r rs))], [])) -- or
exeScenario 13 = ((1,1,0), (0,0,1,0), \[[rd, rs]] [r] _ -> ([], [], [replaceV r rd (xor (atV r rd) (atV r rs))], [])) -- xor
exeScenario 14 = ((1,1,0), (1,0,1,0), \[[rs, v]] [r] _ -> ([if atV r rs == 0 then v else 0], [], [r], []))            -- bez
exeScenario 15 = ((1,1,0), (1,0,1,0), \[[rs, v]] [r] _ -> ([if atV r rs /= 0 then v else 0], [], [r], []))            -- bnz
exeScenario 16 = ((1,1,0), (1,0,1,0), \[[rs, v]] [r] _ -> ([if atV r rs > 0 then v else 0], [], [r], []))             -- bgz
exeScenario 17 = ((1,1,0), (1,0,1,0), \[[rs, v]] [r] _ -> ([if atV r rs < 0 then v else 0], [], [r], []))             -- blz
exeScenario 18 = ((1,0,0), (1,0,0,0), \[[v]] _ _ -> ([v], [], [], []))                                                -- jmp
exeScenario 19 = ((1,1,0), (0,1,1,0), \[[rs]] [r] _ -> ([], [atV r rs], [r], []))                                     -- outr
exeScenario 20 = ((1,0,1), (0,1,0,1), \[[mn]] _ [m] -> ([], [atV m mn], [], [m]))                                     -- outm
exeScenario _  = error "exeScenario: Non existent scenario"

-- | 'exeKernel' is the Execution (EXE) kernel process
exeKernel :: Signal ScenarioEXE -> Signal [Int] -> (Signal Int, Signal Int)
exeKernel exeCt sigArg = (sigBr, sigDmp)
  where (sigBr, sigDmp, sigReg, sigDm) = kernel34SADF exeCt sigArg sigReg' sigDm'
        sigReg' = delaySADF [regV] sigReg
        sigDm' = delaySADF [memV] sigDm

regV = vector $ replicate 32 0
memV = vector $ replicate 1024 0

---------------------------------------------------------
-- Decode Detector
---------------------------------------------------------

type ScenarioDEC = ((Int,Int), ([ScenarioIF], [ScenarioEXE]))

type StateDEC = Int

-- | 'detectorScenario' is th output function of the detector. It converts a
-- state into an equivalent scenario
decScenario :: StateDEC -> ScenarioDEC
decScenario n = ((1,1), ([ifScenario n], [exeScenario n]))

-- | 'decSwitchState' is the state transition function of the detector
decSwitchState :: StateDEC -> [String] -> StateDEC
decSwitchState _ ["nop"]  = 0
decSwitchState _ ["ld"]   = 1
decSwitchState _ ["ldr"]  = 2
decSwitchState _ ["st"]   = 3
decSwitchState _ ["str"]  = 4
decSwitchState _ ["mov"]  = 5
decSwitchState _ ["movi"] = 6
decSwitchState _ ["add"]  = 7
decSwitchState _ ["sub"]  = 8
decSwitchState _ ["mul"]  = 9
decSwitchState _ ["div"]  = 10
decSwitchState _ ["and"]  = 11
decSwitchState _ ["or"]   = 12
decSwitchState _ ["xor"]  = 13
decSwitchState _ ["bez"]  = 14
decSwitchState _ ["bnz"]  = 15
decSwitchState _ ["bgz"]  = 16
decSwitchState _ ["blz"]  = 17
decSwitchState _ ["jmp"]  = 18
decSwitchState _ ["outr"] = 19
decSwitchState _ ["outm"] = 20
decSwitchState _ _ = error "decSwitchState: Input not recognized"

-- | 'decodeDetector' is the detector of ProSyDe
decDetector :: Signal String -> (Signal ScenarioIF, Signal ScenarioEXE)
decDetector = detector12SADF 1 decSwitchState decScenario 0


---------------------------------------------------------
-- ProSyDe compact
---------------------------------------------------------

-- | 'prosyde' is the processor built with the ForSyDe SADF library. It outputs
-- whatever it is in register @rs@ during the execution of instruction @outr rs@
procNet :: Vector String -> Signal Int
procNet progV = sigDmp
  where (sigBr, sigDmp) = exeKernel exeCt sigArg
        (sigArg, sigOp) = ifKernel progV ifCt' sigBr
        (ifCt, exeCt) = decDetector sigOp
        ifCt' = delaySADF [ifScenario 0] ifCt


