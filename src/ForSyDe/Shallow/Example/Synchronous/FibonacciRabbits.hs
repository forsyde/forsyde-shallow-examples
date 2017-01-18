-----------------------------------------------------------------------------
-- |
-- Module      :  FibonacciRabbits
-- Copyright   :  José Edil Guimarães de Medeiros
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  j.edil@ene.unb.br
-- Stability   :  experimental
-- Portability :  portable
--
-- In 1202 Fibonacci posed and solved the following problem:
--
-- >>> A certain man put a pair of rabbits in a place surrounded on
-- >>> all sides by a wall. How many pairs of rabbits can be produced
-- >>> from that pair in a year if it is supposed that every month
-- >>> each pair begets a new pair which from teh second month on
-- >>> becames productive?
--
-- This is a typical discrete-time, linear, time-invariant system. This
-- demo develops a Synchronous version of thiz puzzle.
--
-- =Mathematical formulation
--
-- The system consists of the the number of rabbit pairs and the rules
-- of rabbit reproduction. The output signal @f[n]@ will represent the
-- number of rabbits at month @n@.
--
-- Each month, a pair of rabbits generate a new pair, but each new
-- pair has to wait for one month to be able to generate a new
-- pair. Finding @f[n]@ requires that we consider the number of
-- newborn rabbits separately from the productive adults. The number
-- of rabbits @f[n]@ at a given month @n@ will them be given by:
--
-- >>> f[n] = newborns[n] + adults[n]
--
-- This is a combinational process as the output depends only on the
-- actual value of the inputs.
--
-- In a given month @n@, there are @adults[n]@ productive rabbits that
-- will generate newborns for the next month, that is:
--
-- >>> newborns[n] = adults[n-1]
--
-- Next, the number of adults at a month @n@ will be given by the
-- number of adults the month before plus the number of newborns that
-- became adults.
--
-- >>> adults[n] = adults[n-1] + newborns[n-1]
--
-- =ForSyDe modeling
--
-- Note that @newborns[n]@ is a delayed version of
-- @adults[n]@ and there is one initial pair of newborns at time @n =
-- 0@. 'newborns' is a process that implements it.
--
-- @adults[n]@, on the other hand, keeps track of the number of
-- newborns and adults and can be modeled by a finite state
-- machine. If we choose the state to keep track of the number of
-- adults we can choose a Moore model in which the next state depends
-- on the actual state and on the inputs but the outputs depend only
-- on the current state. 'adults' is a process that implements it.
--
-- Finally, the top level module called 'fibonacciRabbits' calculates
-- the output @f[n]@. It is implemented with a zipWith3SY process
-- constructor so we can keep track of the input signal 'ticks'. Each
-- token in 'ticks' represents a month in the model. So, to calculate
-- what happens with @f[n]@ for the first 12 months, we would call
--
-- >>> fibonnaciRabbits $ signal [1..12]
-- {1,1,2,3,5,8,13,21,34,55,89,144}
--
-- =Running the demo
-- 
-- To run the demo you need the
-- <http://hackage.haskell.org/package/ForSyDe ForSyDe package>
-- installed in your environment. For plotting, you need the
-- <http://gnuplot.sourceforge.net Gnuplot package>.
--
-- To simulate the system for 12 months, run in @ghci@:
--
-- >>> simulate 12
-- 
-- To plot the number of rabbits evolution in 12 months, run in @ghci@:
--
-- >>> plotOutput 12 0.1
-----------------------------------------------------------------------------

module ForSyDe.Shallow.Example.Synchronous.FibonacciRabbits where

import ForSyDe.Shallow

-- | 'fibonacciRabbits' is the top-level process that calculates
-- consume a token on the input and calculate the sequence @f[n]@ for
-- each token.
fibonacciRabbits :: Signal a    -- ^ 'ticks' signal. Each token
                                -- represents one month on the model.
                 -> Signal Integer -- ^ @f[n]@, the number of rabbits.
fibonacciRabbits ticks = zipWith3SY add n a ticks
  where n = newborns a
        a = adults n
        add x y ctrl = x + y

-- | 'newborns' calculates @newborns[n+1]@ given @adults[n]@
newborns :: Signal Integer      -- ^ input signal, @adults[n]@
         -> Signal Integer      -- ^ output signal, @newborns[n+1]@
newborns = delaySY 1

-- | 'adults' calculates @adults[n+1]@  given @newborns[n]@
adults :: Signal Integer        -- ^ input signal, @newborns[n]@
       -> Signal Integer        -- ^ output signal, @adults[n+1]@ 
adults = mooreSY nsf out 0
  where nsf state input = (state + input)
        out state = state

-- | 'simulate' takes the number of months and simulates the system's behavior.
simulate :: Integer             -- ^ number of months to simulate
         -> Signal Integer      -- ^ @f[n]@ sequence
simulate months = fibonacciRabbits $ signal [1..months]

-- | 'plotOutput' uses the CTLib plot capabilities to plot the
-- output. In a later version, a plotter to Synchonous signals will be
-- developed.
plotOutput :: Integer           -- ^ number of months to simulate
           -> Rational          -- ^ timestep
           -> IO String         -- ^ plot
plotOutput months timestep = plotCT' timestep [(output, "output")]
  where output = d2aConverter DAhold 1.0 $
                 mapSY (fromInteger) $
                 fibonacciRabbits $ signal [1..months]
