-----------------------------------------------------------------------------
-- |
-- Module      :  FlashADC
-- Copyright   :  José Edil Guimarães de Medeiros
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  j.edil@ene.unb.br
-- Stability   :  experimental
-- Portability :  portable
--
-- 
--
-- =Mathematical formulation
--
-- 
-- =ForSyDe modeling
--
-- 
-- =Running the demo
-- 
-- To run the demo you need the ForSyDe installed in your
-- environment. For plotting, you need the
-- <http://gnuplot.sourceforge.net Gnuplot package>.
--
-- To calculate 10000 samples for R=1e3 Omhs, C=1e-6 F, T=1e-6run in @ghci@:
--
-- >>> simulate 1e3 1e-6 1e-6 10000
-- 
-- To plot the response for a square wave input, run in @ghci@:
--
-- >>> plotOutput 1e3 1e-6 1e-6 10000
-----------------------------------------------------------------------------

module ForSyDe.Shallow.Example.Synchronous.FlashADC where

import ForSyDe.Shallow

-- | 'flashADC' is the top level module.
flashADC :: [Double]                    -- ^ Resistance values
          -> Signal Double              -- ^ Input signal
          -> Signal Integer             -- ^ Output signal
flashADC resistors input = decoder $ compNetwork input $ resNetwork resistors

-- | 'decoder' takes the thermometer code and outputs integers.
decoder :: [Signal Integer]             -- ^ Bit inputs
        -> Signal Integer               -- ^ Output signal
decoder = foldl1 (zipWithSY (+))

-- | 'compNetwork' implements the comparator array.
compNetwork :: Signal Double            -- ^ ADC input signal
            -> [Double]                 -- ^ Voltage thresholds
            -> [Signal Integer]         -- ^ Bit outputs
compNetwork input = zipWith (\i v -> mapSY (comparator v) i) (repeat input)

-- | 'comparator' is the one bit quantizer.
comparator :: Double                    -- ^ (+) input
           -> Double                    -- ^ (-) input
           -> Integer                   -- ^ Output
comparator i v
  | v <= i = 0
  | otherwise = 1

-- | 'resNetwork' implements the resistor voltage scaling network.
resNetwork :: [Double]                  -- ^ Resistor values
           -> [Double]                  -- ^ Threshold voltages
resNetwork resistors = init $ tail $ scanl (\v r -> v + vdd * r / (sumR)) 0 resistors
  where vdd = 1
        sumR = sum resistors

-- | 'simulate' takes the system parameters and runs the simulation
-- with a sine wave input.
simulate :: Int                         -- ^ ADC resolution
         -> Signal Integer              -- ^ ADC output
simulate res = flashADC resistors input
  where resistors = replicate (2^res) 1
        input = signal $ map (\t -> 0.5*sin(2*pi*t)+0.5) linspace
        linspace = map (\t -> t/100) [1..100]

-- | 'plotOutput' uses the CTLib plot capabilities to plot the
-- output. In a later version, a plotter to Synchornous signals will be
-- developed.
plotOutput :: Int                       -- ^ Resolution
           -> Double                    -- ^ Discretization timestep
           -> Int                       -- ^ Number of samples
           -> IO String                 -- ^ plot
plotOutput res t stop = plotCT' (toRational t) [(output, "output")]
  where output = d2aConverter DAhold (toRational t) adcSignal
        adcSignal = signal $ map (toRational) $ fromSignal $ flashADC resistors input
        resistors = replicate (2^res) 1
        input = signal $ map (\t -> 0.5*sin(2*pi*t)+0.5) linspace
        linspace = map (\t -> t/1000) [1..1000]