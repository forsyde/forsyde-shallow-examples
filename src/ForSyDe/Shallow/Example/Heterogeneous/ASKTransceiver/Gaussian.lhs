\chapter{The module \haskell{Gaussian}}

\begin{code}

module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Gaussian where


-- import SynchronousLib
-- import UntimedLib
-- import BitVector
-- import Signal

import ForSyDe.Shallow
import ForSyDe.Shallow.SynchronousLib
import ForSyDe.Shallow.UntimedLib
import ForSyDe.Shallow.BitVector
import ForSyDe.Shallow.Signal

import System.Random
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Utilities

-- Test case
sig_GaussianDouble1 = mapU 2 (gaussianF1 0 3e-9 ) sig_myRand   
sig_myRand = myRand (11206341::Integer) (64::Int) tapsLFSR
-- /Here is the Gaussian noise algorithms, which is the same as SystemC
-- group's.
gaussianF1 mean var [x1 ,x2 ]
  | q > 1.0 = []
  | otherwise = [mean +  sqrt(var) * sqrt(-2*log q/q) * q1]
  where
    rnd1 = fromIntegral x1 / maxRand
    rnd2 = fromIntegral x2 / maxRand
    q1 = 2.0 * rnd1 - 1.0
    q2 = 2.0 * rnd2 - 1.0
    q = q1 * q1 + q2 * q2
-- /It is the same algorithms as above, except the data type and to be used for 
-- 'zipWithU 1 2' process constructor.
gaussianF2 mean [var] [x1 ,x2 ]
  | q > 1.0 = []
  | otherwise = [mean +  sqrt(var) * sqrt(-2*log q/q) * q1]
  where
    rnd1 = fromIntegral x1 / maxRand
    rnd2 = fromIntegral x2 / maxRand
    q1 = 2.0 * rnd1 - 1.0
    q2 = 2.0 * rnd2 - 1.0
    q = q1 * q1 + q2 * q2


-- We follows the Box-Muller Method to Generate White Gaussian Noise
-- described at: http://www.dspguru.com/howto/tech/wgn.htm

-- uniform returns a random variable in the range [0, 1]
uniform :: (Fractional a, RandomGen g, Random a) => g -> (a, g)
uniform rGen = randomR (0.0,1.0) rGen

-- To generate an infinite Signal of unit normal random variables,
-- with the specified seed
pUnitNormXY :: Int -> Signal Double
pUnitNormXY = mapU 3 unitNormXY . signal . svGenerator . mkStdGen
  where
    unitNormXY [s, v1, v2] = [sqrt(-2 * log(s) / s) * v1,
                              sqrt(-2 * log(s) / s) * v2]

-- To generate an infinite Signal of Gaussian values
pGaussianNoise:: Double -- Mean value of the Gaussian noise
            -> Double -- Variance of the Gaussian noise
            -> Int    -- The seed
            -> Signal Double
pGaussianNoise mean variance = mapU 2 gaussianXY . pUnitNormXY
  where
    gaussianXY [x, y] = [mean + sqrt(variance) * x,
                         mean + sqrt(variance) * y]

-- To generate the s, v1, v2 value
svGenerator :: StdGen -> [Double]
svGenerator s
     | sVal >=1 = []++ svGenerator newStdG
    | otherwise = svVal ++ svGenerator newStdG
  where
    svGen1 = svHelper  s
    svVal = fst svGen1
    sVal = head svVal
    newStdG = snd svGen1
    svHelper :: StdGen -> ([Double], StdGen)
    svHelper stdG = ([s, v1, v2], sNew2)
      where
        (u1, sNew1) = uniform stdG
        (u2, sNew2) = uniform sNew1
        v1=2 * u1 -1
        v2=2 * u2 -1
        s = v1*v1 + v2*v2

-- Test case
-- A Gaussian Noise signal with mean 0.0, variance 2.0 and seed 1
s1 = takeS 20 $ pGaussianNoise 0.0 2.0 1
\end{code}


\begin{code}
-- Constants
lenLFSR = 64 :: Int
-- lenLFSR = 32 :: Int
initialShiftRegBit = 0 :: Int
tapsLFSR = [59,60,62,63] :: [Int]
-- tapsLFSR = [26,29,30,31] :: [Int]
initShiftReg = repeatN lenLFSR initialShiftRegBit
-- The length of the droped sequence at the start of PRNG
dropLen = 20000 :: Int
-- Max random number
maxRand = 2147483647 :: Double

----------------------------------------------------------
-- Test case
testGaussianNoise1 = takeS 100 $
          zipSY (signal [1..]) $ 
          gaussianNoiseGen 0.0 (signal $ repeat  3e-9 ) $
          myRandDouble (4875297544111206341::Integer) (64::Int) tapsLFSR maxRand
-- |Gaussian noise generator, with variable variances.
gaussianNoiseGen :: Double
              -> Signal Double
              -> Signal Double
              -> Signal Double
gaussianNoiseGen mean sigVariance sigRandDouble = sigG
  where
    sigQ = mapU 2 calQ sigRandDouble
    sigG = zipWithSY (calG mean) sigVariance sigQ

-- Helper function
calG mean variance (q,q1) = 
  mean +  sqrt(variance) * sqrt(-2*log q/q) * q1
calQ [rnd1,rnd2] | q > 1.0 = []
                 | otherwise = wrap $ (q,q1)
  where
    q = q1*q1 + q2*q2
    q1 = 2.0*rnd1 -1.0
    q2 = 2.0*rnd2 -1.0

----------------------------------------------------------
-- Test case
testMyDouble2 = takeS 100 $ 
    zipSY (signal [1..]) $ 
     myRandDouble (4875297544111206341::Integer) (64::Int) tapsLFSR maxRand
myRandDouble0 = myRandDouble (4875297544111206341::Integer) 
                             (64::Int) tapsLFSR maxRand
-- |The random Double number between (0,1) PRNG.
myRandDouble ::  
           Integer -- ^Seed of the LFSR
      ->  Int    -- ^Length of the LFSR
      -> [Int]   -- ^taps of the LFSR
            ->  Double  -- ^The maximum Int of the PRNG 
            -> Signal Double -- ^Output PRNG signal
myRandDouble  seed lenLFSR tapsLFSR  maxRand = outRandDouble
  where
    randIntS = myRand seed lenLFSR tapsLFSR
    outRandDouble = mapSY (\x -> fromIntegral x / maxRand) randIntS

testIntToBitVector =  fromVector $ intToBitVector 64 (4875297544111206341::Integer)
----------------------------------------------------------
-- Test case
testMyRand1 = takeS 100 $ 
    zipSY (signal [1..]) $ myRand (4875297544111206341::Integer) (64::Int) tapsLFSR
-- |The random int number PRNG.
myRand :: Integer -- ^Seed of the LFSR
      ->  Int    -- ^Length of the LFSR
      -> [Int]   -- ^taps of the LFSR
      -> Signal Int
myRand seed lenLFSR tapsLFSR = outRandInt
  where
    initShiftReg = reverse $ map fromInteger $ fromVector $ intToBitVector lenLFSR seed
    outRandInt = myRand' initShiftReg tapsLFSR


-- Test case
testMyRandInt1 = takeS 1025  $ zipSY (signal [1..]) (myRand' initShiftReg tapsLFSR) 
-- Helper function of `myRand'
myRand' :: [Int] -> [Int] -> Signal Int
myRand' initShiftReg tapsLFSR = 
        fst newLFSR :- (myRand' (snd newLFSR) tapsLFSR)
  where
    newLFSR = valLFSR initShiftReg tapsLFSR


-- |The LFSR module.
-- valLFSR :: [Int] -> [Int] -> (Int,[Int])
valLFSR initShiftReg tapsLFSR = (val,newShiftReg)
  where
    -- Values at the taps
    valuesAtTaps = map (\x -> atL (x+1) initShiftReg) tapsLFSR
    -- Feedback calculation
    zw | snd $ foldl helperF1 (head valuesAtTaps,True) (tail valuesAtTaps) = 1
       | otherwise = 0
    -- New shift registers
    newShiftReg = finiteRegList (length initShiftReg) initShiftReg zw
    -- Discard half data from the registers
    halfShiftReg = mapU 2 (\xs -> wrap $ head xs) (signal $ newShiftReg)
    -- Bits 1 to 31 are used to calculate the output PRN
    val = foldlSY helperF2 0 (zipSY (signal [0..30]) halfShiftReg)
    -- Helper functions
    helperF1 (a,boolInit) b | a == b = (1,True) 
                           | otherwise = (0,False) 
    helperF2 valOld (i,0) = valOld
    helperF2 valOld (i,1) = valOld + 2^i

\end{code}

