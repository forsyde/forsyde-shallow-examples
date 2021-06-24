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
-- This module shows the model of a flash architecture ADC.
--
-- =Running the demo
-- 
-- To run the demo you need the ForSyDe installed in your
-- environment. For plotting, you need the
-- <http://gnuplot.sourceforge.net Gnuplot package>.
--
-- To calculate 10000 samples for R=1e3 Ohms, C=1e-6 F, T=1e-6run in @ghci@:
--
-- >>> simulate 4 10000
-- 
-- To plot the response for a sine wave input, run in @ghci@:
--
-- >>> plotOutput 4 0.01 1000
-----------------------------------------------------------------------------

module ForSyDe.Shallow.Example.Synchronous.FlashADC where

import ForSyDe.Shallow
import Data.Complex

type Resistance = Double
type Voltage = Double
type Code = Integer
type Bits = Integer
type Time = Double
type Frequency = Double

-- | 'flashADC' is the top level module.
flashADC :: [Resistance]                -- ^ Resistance values
          -> Signal Voltage             -- ^ Input signal
          -> Signal Code                -- ^ Output signal
flashADC resistors input = decoder $ compNetwork input $ resNetwork resistors

-- | 'decoder' takes the thermometer code and outputs integers.
decoder :: [Signal Bits]                -- ^ Bit inputs
        -> Signal Code                  -- ^ Output signal
decoder = foldl1 (zipWithSY (+))

-- | 'compNetwork' implements the comparator array.
compNetwork :: Signal Voltage           -- ^ ADC input signal
            -> [Voltage]                -- ^ Voltage thresholds
            -> [Signal Bits]            -- ^ Bit outputs
compNetwork input = zipWith (\i v -> mapSY (comparator v) i) (repeat input)

-- | 'comparator' is the one bit quantizer.
comparator :: Voltage                   -- ^ (+) input
           -> Voltage                   -- ^ (-) input
           -> Bits                      -- ^ Output
comparator i v
  | v <= i = 0
  | otherwise = 1

-- | 'resNetwork' implements the resistor voltage scaling network.
resNetwork :: [Resistance]              -- ^ Resistor values
           -> [Voltage]                 -- ^ Threshold voltages
resNetwork resistors = init $ tail $ scanl (\v r -> v + vdd * r / (sumR)) 0 resistors
  where vdd = 1
        sumR = sum resistors

-- | 'simulate' takes the system parameters and runs the simulation
-- with a sine wave input.
simulate :: Int                         -- ^ ADC resolution
         -> Int                         -- ^ Number of samples
         -> Signal Code                 -- ^ ADC output
simulate res n = flashADC resistors input
  where resistors = replicate (2^res) 1
        input = sineWave' 0.5 0.5 50 n

-- | 'plotFFT' runs the simulation with a sine wave input
plotFFT :: Int                  -- ^ ADC resolution
        -> Int                  -- ^ FFT depth (power of 2)
        -> IO String            -- ^ plot
plotFFT res n = plotCT' (toRational 1.0) [(g_out, "g")]
  where g_out = d2aConverter DAhold (toRational 1.0) $ signal g
        a = fromSignal $ simulate res n
        b = (:+) <$> (map ((\x -> x/2^res - 0.5).fromIntegral) a)
        c = zipWith ($) b $ repeat 0.0
        d = vector c
        e = fromVector $ fft n d
        f = map ((*20).(logBase 10).(\x -> 2*x/(fromIntegral n)) . magnitude) e
        g = (take (n `div` 2) f)

-- | 'plotOutput' uses the CTLib plot capabilities to plot the
-- output. In a later version, a plotter to Synchornous signals will be
-- developed.
plotOutput :: Int                       -- ^ Resolution
           -> Double                    -- ^ Discretization timestep
           -> Int                       -- ^ Number of samples
           -> IO String                 -- ^ plot
plotOutput res t n = plotCT' (toRational t) [(output, "output")]
  where output = d2aConverter DAhold (toRational t) adcSignal
        adcSignal = signal $ map (toRational) $ fromSignal $ 
                    simulate res n

-- | 'sineWave' is an auxiliary function that creates a sine wave signal for
-- simulation purposes.
sineWave' :: Voltage                     -- ^ Amplitude
          -> Voltage                     -- ^ Offset
          -> Frequency                   -- ^ Frequency
          -> Int                         -- ^ Number of samples
          -> Signal Time                 -- ^ Output signal
sineWave' amp offset freq n = signal sineW
  where sineW = map (\t -> (amp/2) * sin(2*pi*freq*t) + offset) grid
        grid = linspace 0 1 n

-- | 'linspace' is an auxiliary function that creates an uniform
-- spaced sampling grid for simulation purposes.
linspace :: Time                        -- ^ Start time
         -> Time                        -- ^ Stop time
         -> Int                         -- ^ Number of samples
         -> [Time]                      -- ^ Sampling grid
linspace start stop n = map (scale) nodes
  where n' = fromIntegral n
        nodes = map (fromIntegral) [0..(n-1)]
        scale t = t * length / (n' - 1) + start
        length = stop - start 

