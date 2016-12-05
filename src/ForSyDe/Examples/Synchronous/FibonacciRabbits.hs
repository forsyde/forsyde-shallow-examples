-----------------------------------------------------------------------------
-- |
-- Module      :  PitchControl
-- Copyright   :  José Edil Guimarães de Medeiros
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  j.edil@ene.unb.br
-- Stability   :  experimental
-- Portability :  portable
--
-- This is the Pitch Control example.
-- 
-----------------------------------------------------------------------------

module ForSyDe.Examples.Synchronous.FibonacciRabbits where

import ForSyDe.Shallow

type Tick = Int

-- | top level.
fibonacciRabbits :: Signal a
                -> Signal Integer
fibonacciRabbits ticks = zipWith3SY add c a ticks
  where c = childs a
        a = adults c
        add x y ctrl = x + y

  
-- | childs
childs :: Signal Integer
       -> Signal Integer
childs = delaySY 1

-- | adults
adults :: Signal Integer
       -> Signal Integer
adults = mooreSY nsf out 0
  where nsf state input = (state + input)
        out state = state
