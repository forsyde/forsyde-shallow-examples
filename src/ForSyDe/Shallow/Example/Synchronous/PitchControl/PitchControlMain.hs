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

-- | 'pitchControl' is the top level module.
pitchControl :: Signal Sample   -- ^ System setpoint. 
             -> Signal Sample   -- ^ System output.
pitchControl setPoint = controlSignal
  where
    controlSignal = pitchControlSS pidOutput
    pidOutput = pid (kp, ki, kd) errorSignal
    errorSignal = sigma setPoint measuredSignal
    measuredSignal = sensor controlSignal
    kp = 0.5781
    ki = 0.5420
    kd = 0.4697

-- | 'sensor' is implemented as a delay on the feedback loop.
sensor = delaySY 0.0

-- | 'pid' is the controller model. It is implemented as a Mealy process.
pid :: (Sample, Sample, Sample) -- ^ (kp, ki, kd) parameters.
    -> Signal Sample            -- ^ Input signal.
    -> Signal Sample            -- ^ Output signal.
pid (kp, ki, kd) a = mealySY nextState outputDecoder s0 a
  where
    nextState state _ = state
    outputDecoder state input = kp * input + ki * (input + state) + kd * (input - state)
    s0 = 0.0

-- | 'sigma' is the adder module that compares the setpoint with the feedback signal.
sigma :: Signal Sample          -- ^ (+) input.
      -> Signal Sample          -- ^ (-) input.
      -> Signal Sample          -- ^ Output signal.
sigma a b = zipWithSY (-) a b

-- | 'ctOutput' is an auxiliary process used for plotting. 
ctOutput = d2aConverter DAlinear 1.0 $ pitchControl $ takeS 1000 ones
  where ones = 1.0:-ones 

-- | 'plotCtOutput' runs the simulation.
plotCtOutput = plotCT' 10 [(ctOutput, "Step response PID pitch control")]
