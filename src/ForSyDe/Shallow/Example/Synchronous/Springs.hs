-----------------------------------------------------------------------------
-- |
-- Module      :  Springs
-- Copyright   :  José Edil Guimarães de Medeiros
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  j.edil@ene.unb.br
-- Stability   :  experimental
-- Portability :  portable
--
-- This demo shows some of the numerical issues associated with the
-- analysis of second order systems. We suppose a mass connected to a
-- spring that is free to oscillate in just one dimension. We pull the
-- string in time 0 and let it oscillate freely. Out intuition says
-- that the mass would oscillate forever as we are not considering any
-- loss. The differential equations that describes the mass
-- displacement is:
--
-- x'' + c/m * x = 0
--
-- in which @x''@ means the second derivative of the position @x@, @c@ is
-- the spring constant and m is the mass attached to the spring.
--
-- =Mathematical formulation
--
-- Here we face 3 possibilities for a first order approximation to the
-- derivative. The first is to look forward, that is
--
-- x'[k] = 1/T * (x[k+1] - x[n])
--
-- which means
--
-- x''[k] = 1/T^2 * (x[k+2] - 2*x[k+1] + x[k]).
--
-- Plug it in the differential equation and after some algebraic
-- manipulation we get
--
-- x[k] = 2*x[k-1] - (1 + T^2*c/m)*x[k-2]
-- 
-- Next, the second possibility is to look backwards on the derivative
-- approximation, that is
--
-- x'[k] = 1/T * (x[k] - x[n-1])
--
-- which lead to
--
-- x''[k] = 1/T^2 * (x[k] - 2*x[k-1] + x[k-2]).
--
-- After plugging it in the differetial equation we get
--
-- x[k] = 1/(1+T^2*c/m) * (2*x[k-1] - x[k-2])
--
-- This is a completely different model with different properties as
-- we will analyse later. The last option available is to take a
-- symmetrical approach to the decond derivative approximation which
-- leads to
--
-- x''[k] = 1/T^2 * (k[k+1] - 2*x[k] + x[k-1]).
--
-- Plugging it into the differential equations gives
--
-- x[k] = (2 - T^2*c/m)*x[k-1] - x[k-2]
--
-- =ForSyDe modeling
--
-- All models are modeled as Moore state machines as the output
-- depends on the last two states but there is no input to the system
-- besides the initial mass displascement condition.
-- 
-- =Running the demo
-- 
-- To run the demo you need the
-- <http://hackage.haskell.org/package/ForSyDe ForSyDe package>
-- installed in your environment. For plotting, you need the
-- <http://gnuplot.sourceforge.net Gnuplot package>.
--
-- To plot the outputs for the three models with a 1kg mass, 10N/m
-- spring constant, 0.1s timestep, 1m displacement and 2000 samples,
-- run in @ghci@
--
-- >>> plotOutput 1 10 0.1 1 2000
-----------------------------------------------------------------------------

module ForSyDe.Shallow.Example.Synchronous.Springs where

import ForSyDe.Shallow

-- | 'state' type to save the last two outputs of the systems.
type State = (Double, Double)

-- | 'forwardSpring' implements the forward looking method described.
forwardSpring :: Double         -- ^ spring constant, @c@
              -> Double         -- ^ mass, @m@
              -> Double         -- ^ discretization timestep, T
              -> Double         -- ^ initial displacemet
              -> Signal Integer -- ^ tokens to be consumed by the model
              -> Signal Double  -- ^ mass position, @x[k]@
forwardSpring c m t x0 = mooreSY nsf out (x0,x0)
  where nsf (x1, x2) input = (out (x1,x2), x1)
        out (x1, x2) = 2*x1 - (1 + t^2*c/m)*x2

          
-- | 'backwardspring' implements the backward looking method described.
backwardSpring :: Double         -- ^ spring constant, @c@
               -> Double         -- ^ mass, @m@
               -> Double         -- ^ discretization timestep, T
               -> Double         -- ^ initial displacemet
               -> Signal Integer -- ^ tokens to be consumed by the model
               -> Signal Double  -- ^ mass position, @x[k]@
backwardSpring c m t x0 = mooreSY nsf out (x0,x0)
  where nsf (x1, x2) input = (out (x1,x2), x1)
        out (x1, x2) = 1/(1 + t^2*c/m) * (2*x1 - x2)

-- | 'symmetricalSpring' implements the symmetrical method described.
symmetricalSpring :: Double         -- ^ spring constant, @c@
                  -> Double         -- ^ mass, @m@
                  -> Double         -- ^ discretization timestep, T
                  -> Double         -- ^ initial displacemet
                  -> Signal Integer -- ^ tokens to be consumed by the model
                  -> Signal Double  -- ^ mass position, @x[k]@
symmetricalSpring c m t x0 = mooreSY nsf out (x0,x0)
  where nsf (x1, x2) input = (out (x1,x2), x1)
        out (x1, x2) = (2 - t^2*c/m)*x1 - x2

-- | 'plotOutput' plots the three different models outputs to
-- comparison of the integration methods.
plotOutput :: Double            -- ^ spring constant, @c@
           -> Double            -- ^ mass, @m@
           -> Double            -- ^ timestep
           -> Double            -- ^ initial displacemet
           -> Integer           -- ^ number of samples
           -> IO String         -- ^ plot
plotOutput c m t x0 samp = plotCT' (toRational t) $
                           [(forSpring, "Forward model"),
                            (backSpring, "Backward model"),
                            (symSpring, "Symmetrical model")]
  where forSpring = d2aConverter DAhold (toRational t) $
                    forwardSpring c m t x0 $ signal [1..samp]
        backSpring = d2aConverter DAhold (toRational t) $
                     backwardSpring c m t x0 $ signal [1..samp]
        symSpring = d2aConverter DAhold (toRational t) $
                    symmetricalSpring c m t x0 $ signal [1..samp]
                   
