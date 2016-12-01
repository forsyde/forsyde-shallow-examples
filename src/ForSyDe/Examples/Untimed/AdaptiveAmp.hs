{-# OPTIONS_HADDOCK prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  AdaptiveAmp
-- Copyright   :  José Edil Guimarães de Medeiros
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  j.edil@ene.unb.br
-- Stability   :  experimental
-- Portability :  portable
--
-- This is the untimed model for the amplifier example developed throughout
-- chapter 3 of Axel's book Modeling Embedded Systems and SoC's.
-- A block diagram can be found in Figure 3-7 of that book.
-- 
-----------------------------------------------------------------------------

module ForSyDe.Examples.Untimed.AdaptiveAmp (adaptiveAmp,
                                            p1,
                                            p2,
                                            p3,
                                            p4) where

import ForSyDe.Shallow

-- | 'adaptiveAmp' is the top level process. It takes a sinal of numbers and
-- amplifies it.
--
-- >>> let sig = signal [1..20]
-- >>> adaptiveAmp sig
-- {10,20,30,40,50,66,77,88,99,110,121,132,143,154,165,160,170,180,190,200}
adaptiveAmp :: (Ord a, Num a)
            => Signal a -- ^ Input signal
            -> Signal a -- ^ Output signal
adaptiveAmp sin = sout
    where   s1 = p1 s3 sin -- Process p1
            sout = p2 s1   -- Process p2
            s2 = p3 sout   -- Process p3
            s3 = p4 s2     -- Process p4

-- | Process 'p1': zips the input and the control signal
p1 :: Num a
  => Signal a          -- ^ Control signal
  -> Signal a          -- ^ Data signal
  -> Signal ([a], [a]) -- ^ Output signal
p1 = zipUs 1 5

-- | Process 'p2': amplifier
p2 :: Num a
  => Signal ([a], [a]) -- ^ Input signal
  -> Signal a          -- ^ Amplified signal
p2 = mapU 1 mult
    where   mult [([control], signal)] = map (* control) signal

-- | Process 'p3': takes 5 tokens and decides if the output is
-- too big to generate the control signal.
p3 :: (Ord a, Num a)
  => Signal a -- ^ Data signal
  -> Signal a -- ^ Control signal
p3 = scanU (\_ -> 5) g 10  
                where   g :: (Ord a, Num a) => a -> [a] -> a
                        g state signal 
                            | sum signal > 500  = state - 1
                            | sum signal < 400  = state + 1
                            | otherwise         = state

-- | Process 'p4': delays the control signal to guarantee an initial
-- value of 10.
p4 :: (Num a)
  => Signal a -- ^ Data signal
  -> Signal a -- ^ Delayed signal
p4 = initU [10]            
