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

module ForSyDe.Shallow.Example.Synchronous.PitchControl (
  module ForSyDe.Shallow.Example.Synchronous.PitchControl.PitchControlMain,
  module ForSyDe.Shallow.Example.Synchronous.PitchControl.PitchControlSS
  ) where

import ForSyDe.Shallow.Example.Synchronous.PitchControl.PitchControlMain
import ForSyDe.Shallow.Example.Synchronous.PitchControl.PitchControlSS
