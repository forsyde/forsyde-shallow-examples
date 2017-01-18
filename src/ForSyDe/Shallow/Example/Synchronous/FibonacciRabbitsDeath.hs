-----------------------------------------------------------------------------
-- |
-- Module      :  FibonacciRabbitsDeath
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
-- We solved this puzzle in example
-- 'FibonacciRabbits' demo. Here we consider a modified version of
-- the problem in which the rabbits die after 4 months.
-- 
-- =Mathematical formulation
--
-- Now, we need to keep track of the number of dead rabbits each
-- month, @dead[n]@. The number of dead rabbits in a month will be the
-- same number of newborns, but 4 months delayed:
--
-- >>> dead[n] = newborns[n-4]
--
-- Also, the number of rabbits in a given month have to consider the
-- death rate:
--
-- >>> f[n] = adults[n] + newborns[n] - dead[n]
--
-- =ForSyDe modeling
--
-- We add a process dead to keep track of the death rate.
--
-- Also, the top level module called 'fibonacciRabbits' now has to
-- consider an additional input
--
-- >>> fibonnaciRabbitsDeath $ signal [1..12]
-- {1,1,2,3,4,8,12,20,32,52,84,136}
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

module ForSyDe.Shallow.Example.Synchronous.FibonacciRabbitsDeath where

import ForSyDe.Shallow

-- | 'fibonacciRabbits' is the top-level process that calculates
-- consume a token on the input and calculate the sequence @f[n]@ for
-- each token.
fibonacciRabbitsDeath :: Signal a    -- ^ 'ticks' signal. Each token
                                -- represents one month on the model.
                      -> Signal Integer -- ^ @f[n]@, the number of rabbits.
fibonacciRabbitsDeath ticks = zipWith4SY fusion n a d ticks
  where n = newborns a
        a = adults n
        d = dead n
        fusion x y z ctrl = x + y - z

-- | 'newborns' calculates @newborns[n+1]@ given @adults[n]@.
newborns :: Signal Integer      -- ^ input signal, @adults[n]@
         -> Signal Integer      -- ^ output signal, @newborns[n+1]@
newborns = delaySY 1

-- | 'adults' calculates @adults[n+1]@  given @newborns[n]@.
adults :: Signal Integer        -- ^ input signal, @newborns[n]@
       -> Signal Integer        -- ^ output signal, @adults[n+1]@ 
adults = mooreSY nsf out 0
  where nsf state input = (state + input)
        out state = state

-- | 'dead' calculates @dead[n]@ given @newborns[n]@.
dead :: Signal Integer          -- ^ input signal, @newborns[n]@
     -> Signal Integer          -- ^ output signal, @dead[n+4]@
dead = delaynSY 0 4

-- | 'simulate' takes the number of months and simulates the system's behavior.
simulate :: Integer             -- ^ number of months to simulate
         -> Signal Integer      -- ^ @f[n]@ sequence
simulate months = fibonacciRabbitsDeath $ signal [1..months]

-- | 'plotOutput' uses the CTLib plot capabilities to plot the
-- output. In a later version, a plotter to Synchonous signals will be
-- developed.
plotOutput :: Integer           -- ^ number of months to simulate
           -> Rational          -- ^ timestep
           -> IO String         -- ^ plot
plotOutput months timestep = plotCT' timestep [(output, "output")]
  where output = d2aConverter DAhold 1.0 $
                 mapSY (fromInteger) $
                 fibonacciRabbitsDeath $ signal [1..months]
