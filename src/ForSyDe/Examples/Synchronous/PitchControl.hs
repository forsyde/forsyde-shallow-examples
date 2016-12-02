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

module ForSyDe.Examples.Synchronous.PitchControl where

import ForSyDe.Shallow

type Param = Double
type Sample = Double

-- | The PID has to keep track of its history.
type State = (Sample,           -- u[k-1]
              Sample,           -- e[k-1]
              Sample)           -- e[k-2]

pitchControl :: Signal Sample   -- ^ Reference signal
             -> Signal Sample   -- ^ System response
pitchControl setPoint = controlSignal
  where
    controlSignal = plant pidOutput
    pidOutput = pid (kp, ki, kd) errorSignal
    errorSignal = sigma setPoint measuredSignal
    measuredSignal = sensor controlSignal
    kp = 0.5021
    kd = 0.22
    ki = 0.1198

-- sensor has delay to break the 0 delay in the sensor feedback loop
sensor :: Signal Sample
       -> Signal Sample
sensor = delaySY 0.0

plant :: Signal Sample
      -> Signal Sample
plant = mapSY id

pid :: (Param, Param, Param)
    -> Signal Sample
    -> Signal Sample
pid (kp, ki, kd) as = mealySY nextState outputDecoder (0.0,0.0,0.0) as
  where
    --nextState :: State -> Signal -> State
    nextState (u,e1,e2) input = (out,input,e1)
      where out = u + a*input + b*e1 + c*e2
    outputDecoder (u,e1,e2) input = u
    --outputDecoder _ _ = 1
    a = kp + ki * ts/2 + kp/ts
    b = -kp + ki * ts/2 - 2 * kd/ts
    c = kd/ts
    ts = 1

sigma :: Signal Sample
      -> Signal Sample
      -> Signal Sample
sigma as bs = zipWithSY (-) as bs


ctOutput = d2aConverter DAhold 0.1 $ pitchControl $ ((takeS 10 zeros) +-+ (takeS 100 ones))
  where ones = 1.0:-ones
        zeros = 0.0:-zeros

plotCtOutput = plotCT' 0.1 [(ctOutput, "ctOutput")]
