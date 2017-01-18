/\chapter{The Module \haskell{Parameters}}

The module \haskell{Parameters} gathers the parameters used in the 
ASK transceiver system example. 
\begin{code}
module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Parameters where

import Data.Ratio

freq_byte = 1000  -- 0.001 MHZ
period_byte = 1%freq_byte -- Clock period of one byte

freq_bit = 9000  -- 0.009 MHZ,
period_bit =  1%freq_bit -- Clock period of one bit in the digital domain

-- Frequency of ASK sine signal is 10MHZ
freq_ASK = 100000 -- 00 -- 1e7
radian_sin = toRational $ 2*pi* (fromInteger freq_ASK)

-- The attenuation of the ouput analog signal as it is sent back by the TB
attenuation = 0.0001 :: Double

-- Threshold value of detect one
threshVal = 0.000045 

-- It is used to only detect the 2nd sample, which is in the middle of the bit
a2dResolution = 1/2 * period_bit 
samplesPerBit = 2 :: Int

-- Special stuff to trigger bit errors during simulation
change_variance = True
change_var_start = 3 * dataRatePerByte :: Int -- ms
change_var_end = 4* dataRatePerByte :: Int -- ms
change_var_factor = 10.0 -- 1.04 -- 1.07 -- 1.08 -- 1.1  1.04

gaussianVar = 1e-9 -- 3e-9 

freq_cutoff = 3.0*9.0* fromIntegral freq_byte
-- Numberator coefficients of the s-filter is always [1].
-- Denominator coefficients of the s-filter
filterDemCoef :: (Fractional a) => [a]
filterDemCoef = map realToFrac 
                [1/(2*pi*freq_cutoff),1] 

-- One clock cycle in synchronous domain corresponds to 1ms in CT domain
numberOfBits = 9 :: Int         -- number of Bits in a byte
sync2CTClockTime = (toRational numberOfBits ) * period_bit 

-- Gain of the higher power mode
gainPower = 5.0 :: Double
gainThreshold = 2.5 :: Double -- gainPower

-----------------------------------------------
-- Below are some parameters we used different values


-----------------------------------------------
-- Below are some parameters especially used in Our model
-- The input data for test.
integerList = [0..5]

-- Time Interval for the sine source
timeInterval = (0%10,100%1000)

-- Resolution for graphical plots
plotStepSize = 2%100000 -- 000 

-- Sampling period of the filter
-- Now it is 2e-8, that means for each sine period there are 5 samples
a2dResolution' = -- 2%100000000
                 1%dataRate -- 199998000
dataRatePerBit = (floor $ 20 * fromInteger freq_ASK / fromInteger freq_bit) :: Int
dataRatePerByte = 9 * dataRatePerBit :: Int
dataRate = freq_bit * fromIntegral dataRatePerBit
dataPeriod = 1 % dataRate
\end{code}

