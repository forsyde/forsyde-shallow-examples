module ForSyDe.Shallow.Example.SADF.RISC.Test where

import ForSyDe.Shallow
import ForSyDe.Shallow.Example.SADF.RISC.RISC

---------------------------------------------------------
-- Program Code
---------------------------------------------------------

progV :: Vector String
progV = vector [
  "movi 0 1",   -- reg 0 = 1
  "movi 1 1",   -- reg 1 = 1
  "movi 2 100", -- reg 2 = 100
  "add 0 1",    -- reg 0 = reg 0 + reg 1
  "sub 2 0",    -- reg 2 = reg 2 - reg 0
  "bez 2 1",    -- skip next instruction if reg 2 == 0
  "jmp -5",     -- jump back to instruction 3: "movi 2 100"
  "str 0 0",    -- mem(reg 0) = reg 0
  "ldr 10 0",   -- reg 10 = mem(reg 0)
  "outr 10",    -- output reg 10
  "outr 1",     -- output reg 1
  "outm 100",   -- output mem(100)
  "movi 10 7",  -- reg 10 = 7
  "st 10 42",   -- mem(42) = reg 10
  "outm 42",    -- output mem(42)
  "jmp -1"      -- end of program
  ]

progVcomments = [
  "\t# reg 0 = 1",
  "\t# reg 1 = 1",
  "\t# reg 2 = 100",
  "\t\t# reg 0 = reg 0 + reg 1",
  "\t\t# reg 2 = reg 2 - reg 0",
  "\t\t# skip next instruction if reg 2 == 0",
  "\t\t# jump back to instruction 3: \"movi 2 100\"",
  "\t\t# mem(reg 0) = reg 0",
  "\t# reg 10 = mem(reg 0)",
  "\t\t# output reg 10",
  "\t\t# output reg 1",
  "\t# output mem(100)",
  "\t# reg 10 = 7",
  "\t# mem(42) = reg 10",
  "\t\t# output mem(42)",
  "\t\t# end of program"
  ]


testRISC = do
  putStrLn "\nRISC input program:\n"
  putStrLn $ unlines $ zipWith (\p c -> p ++ c) (fromVector progV) progVcomments
  putStrLn "\nOutput sequence:\n"
  print $ takeS 4 $ procNet progV
  
