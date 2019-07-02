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

module ForSyDe.Shallow.Example.SADF.RISC (
  -- * Test Functions and Programs
  testRISC, progV, progVcomments,
  -- * Model of RISC Processor
  module ForSyDe.Shallow.Example.SADF.RISC.RISC
  ) where

import ForSyDe.Shallow.Example.SADF.RISC.RISC
import ForSyDe.Shallow.Example.SADF.RISC.Test
