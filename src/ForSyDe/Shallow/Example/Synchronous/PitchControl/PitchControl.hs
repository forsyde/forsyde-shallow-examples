-----------------------------------------------------------------------------
-- |
-- Module      :  PitchControl
-- Copyright   :  Daniel Mauricio Muñoz Arboleda
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  damuz@unb.br
-- Stability   :  experimental
-- Portability :  portable
--
-- One of the most common control strategies used by industry is the
-- Proportional Integral Derivative (PID) control. It is applied to
-- different fields of applications, from mechanics to electronics
-- systems. PID controllers are easily synthonized to accomplish
-- common systems requirements such as overshoot, rising time, setling
-- time and steady error.
--
-- This example shows how ForSyDe can be used for modelling control
-- systems. We have implemented a digital version of the PID
-- control. In addition, we have selected the continuous
-- time-invariant state-space representation for modelling the plant
-- (see Equation \ref{eqSS}). Fig. presents a general block diagram of
-- the state-space equations. The Euler's method for numerical
-- integration is used. \color{red}{include
-- references}.\color{black}{}
--
-- x'(t) = A*x(t) + B*u(t)
-- y(t) = C*x(t) + D*u(t)
--
-- where $x$ is the state vector, $y$ is the output vector, $u$ is the
-- input or control vector, A is the state matrix, B is the input
-- matrix, C is the output matrix and D is the feethrough matrix.
--
-- A case study is used as a proof of concept. In particular, we have
-- selected the PID controller for the Pitch of a Boeing's commercial
-- aircraft. Details of the system modeling can be found in
--
-- http://ctms.engin.umich.edu/CTMS/index.php?example=AircraftPitch&section=SystemModeling
--
-- For a step reference of 0.2 radians, the design criteria are the following.
-- 1. Overshoot less than 10\%
-- 2. Rise time less than 2 seconds
-- 3. Settling time less than 10 seconds
-- 4. Steady-state error less than 2\%
-- 
-- =Running the demo
-- 
-- To run the demo you need the
-- <http://hackage.haskell.org/package/ForSyDe ForSyDe package>
-- installed in your environment. For plotting, you need the
-- <http://gnuplot.sourceforge.net Gnuplot package>.
--
-- To plot the response of the system with the PID controller, run in @ghci@:
--
-- >>> plotCtOutput
-----------------------------------------------------------------------------

module ForSyDe.Shallow.Example.Synchronous.PitchControl.PitchControl where

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
