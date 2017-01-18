{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  PitchControlMain
-- Copyright   :  Daniel Mauricio Muñoz Arboleda
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  damuz@unb.br
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module ForSyDe.Shallow.Example.Synchronous.PitchControl.PitchControlMain where

import ForSyDe.Shallow
import ForSyDe.Shallow.Example.Synchronous.PitchControl.PitchControlSS

pitchControl :: Signal Sample -> Signal Sample
pitchControl setPoint = controlSignal
  where
    controlSignal = pitchControlSS pidOutput
    pidOutput = pid (kp, ki, kd) errorSignal
    errorSignal = sigma setPoint measuredSignal
    measuredSignal = sensor controlSignal
    kp = 0.5781
    ki = 0.5420
    kd = 0.4697

-- sensor has delay to break the 0 delay in the sensor feedback loop
sensor = delaySY 0.0

pid :: (Sample, Sample, Sample) -> Signal Sample -> Signal Sample
pid (kp, ki, kd) a = mealySY nextState outputDecoder s0 a
  where
    nextState state _ = state
    outputDecoder state input = kp * input + ki * (input + state) + kd * (input - state)
    s0 = 0.0

sigma :: Signal Sample -> Signal Sample -> Signal Sample
sigma a b = zipWithSY (-) a b

ctOutput = d2aConverter DAlinear 1.0 $ pitchControl $ takeS 1000 ones
  where ones = 1.0:-ones 

plotCtOutput = plotCT' 10 [(ctOutput, "Step response PID pitch control")]
