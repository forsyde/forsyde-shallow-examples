\chapter{The module \haskell{Utilities}}
\begin{code}
{-# OPTIONS_HADDOCK hide #-}
module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Utilities where

-- import CTLib
-- import ForSyDeMoCLib
-- import BitVector

import ForSyDe.Shallow
import ForSyDe.Shallow.MoC.CT
import ForSyDe.Shallow.Utility.BitVector

import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Parameters
\end{code}
 
\begin{code}
-- |'subCT' adds two input signals together.
subCT :: (Show a, Num a) =>
         Signal (SubsigCT a) -- ^The first input signal
      -> Signal (SubsigCT a) -- ^The second input signal
      -> Signal (SubsigCT a) -- ^The output signal
subCT s1 s2 = applyF2 f s1' s2'
    where (s1',s2') = cutEq s1 s2
          f g1 g2 = f'
              where f' x = (g1 x) - (g2 x)


constDoubleF :: Double -> Double -> Double
constDoubleF = (\x y->x)

linearDoubleF :: Double -> Double -> Double -> Double -> Double -> Double
linearDoubleF c holdT m n x = (1-alpha)*m + alpha*n
  where alpha = (x-holdT)/c
\end{code}
The function detectBitLevel is used to detect the level of a bit in the ASK signal. The algorithm output a '1', if more that 3 values are larger than the noise threshold value \texttt{noiseThreshVal}. The algorithm should be checked with TU Vienna!
\begin{code}
detectBitLevel :: (Num a, Ord a)=> [a] -> [a] -> [Integer]
detectBitLevel  [x0] [x1,x2]| x2 > x0 = [1]
                            | otherwise = [0]
{-
detectBitLevel x0 xs | sum ( map (isOne x0) xs) > detectOneNum = [1]
                     | otherwise = [0]
  where
    isOne :: (Num a, Ord a) => a -> a -> Int
    isOne x0 x | (abs x) > x0 = 1
            | otherwise = 0
-}
\end{code}
The functions \texttt{wrap} converts a signal value to a singleton list, and \texttt{strip} makes the opposite operation.  
\begin{code}
wrap = \x -> [x]
strip [x] = x
\end{code}
The process \texttt{applyfCT} is an adptive process, where the functionality is controlled by a signal op parameters. The input and output signal are continuous time signals. The first parameter gives the stepsize.
\begin{code}
applyfCT :: (Show a, Num a) => Rational 
         -> Signal ((Rational, Rational), (Rational -> a) -> Rational -> a)
         -> Signal (SubsigCT a)-> Signal (SubsigCT a)
applyfCT c NullS _ = NullS
applyfCT c _ NullS = NullS
applyfCT c (f:-fs) ss = combCTInterval c f (takeCT st'' ss) 
                         +-+ applyfCT c (fs) (dropCT st' ss)
  where
    t' = stopT' f
    s' = startT' f
    st' = t' - s'
    st'' = st' 
    stopT' ((_,t),_) = t
    startT' ((s,_),_) = s    

combCTInterval :: (Show a, Num a) => Rational
               -> ((Rational,Rational),(Rational -> a) -> (Rational -> a))
               -> Signal (SubsigCT a)-> Signal (SubsigCT a)
combCTInterval c ((s,t), f) NullS = NullS
combCTInterval c ((s,t), f) ss
              | (duration (takeCT c ss)) < c = NullS
              | tStart' < s = (takeCT c ss) +-+
                                combCTInterval c ((s,t), f) (dropCT c ss)
              | tStart' > t = NullS 
              | otherwise = applyF1 f (takeCT (t-tStart') ss)
  where
    tStart' = startTime $ takeCT c ss
\end{code}

The process \texttt{adaptiveEncController} is to generate the encoding algorithms. It uses the input signal to control the selection of the output encoding functions.

\begin{code}
adaptiveEncController :: Signal Integer 
                -> Signal (Vector Integer->Vector Integer)
adaptiveEncController NullS = NullS
adaptiveEncController (x:-xs) | x == 1 = encAlgorithm1 :- adaptiveEncController xs
                       | otherwise = encAlgorithm2 :- adaptiveEncController xs
  where
    -- DES
    encAlgorithm1 xs = zipWithV togglingBit xs (vector [1,0,1,0,1,0,1,0])
    -- Blowfish
    encAlgorithm2 xs = zipWithV togglingBit xs (vector [0,1,0,1,0,1,0,1])

togglingBit :: Integer -> Integer -> Integer
togglingBit 0 0 = 0
togglingBit 1 0 = 1
togglingBit 0 1 = 1
togglingBit 1 1 = 0
\end{code}

The process \texttt{adaptiveDecController} is to generate the decoding algorithms. It uses the input signal to control the selection of the output decoding functions.

\begin{code}
adaptiveDecController :: Signal Integer 
                 -> Signal (Vector Integer->Vector Integer)
adaptiveDecController NullS = NullS
adaptiveDecController (x:-xs) | x == 1 = decAlgorithm1 :- adaptiveDecController xs
                       | otherwise = decAlgorithm2 :- adaptiveDecController xs
  where
    -- DES
    decAlgorithm1 xs = zipWithV togglingBit xs (vector [1,0,1,0,1,0,1,0])
    -- Blowfish
    decAlgorithm2 xs = zipWithV togglingBit xs (vector [0,1,0,1,0,1,0,1])


\end{code}

The process \texttt{pAdaptivePowerF} is to generate the power adjustment algorithms. It uses the input signal to control the selection of the different amplititude gain functions to reflect voltage changes.

\begin{code}
adaptivePowerController :: Signal Integer
                        -> Signal ((Rational -> Double) -> Rational -> Double)
adaptivePowerController = mapSY powerF
  where
    powerF i | i==0 = fVL
             | otherwise = fVH
    fVH = \f x -> gainPower * (f x)
    fVL = id

adaptiveThresholdController :: Signal Integer
                            -> Signal Double 
adaptiveThresholdController = mapSY threshF
  where
    threshF i | i==0 = threshVal
             | otherwise = gainThreshold * threshVal
\end{code}
\texttt{sync2CTInterface} is an interface of the signal from the SR domain to CT domain.
\begin{code}
sync2CTInterface :: Rational   -- timestep
                 -> Signal a -- A signal in SR domain
                 -> Signal ((Rational, Rational), a) -- A signal in CT domain
sync2CTInterface c xs = mealySY g f (0::Rational) xs
  where
    g :: Rational -> a -> Rational
    g x _ = x +c
    f :: Rational -> a -> ((Rational, Rational), a)
    f x y = ((x,x+c), y)
\end{code}
\texttt{packBits} is to pack the bits in the signal into bit-vector.
\begin{code}
packBits :: Int -> Signal a -> Signal (Vector a)
packBits n bitSignal = mapU n (wrap . vector) bitSignal 
\end{code}

A Reg list with finite bit size.
\begin{code}
finiteRegList :: Int -- The specified space of the Fifo
               -> [a] -- Previous Fifo state
               ->  a  -- New input
               -> [a] -- Current Fifo state
finiteRegList n xs y = take n (y:xs) 
\end{code}

Some helper functions

\begin{code}
atL n = head . drop (n-1)
-- replaceL n y xs = take (n-1) xs ++ [y] ++ drop n xs
repeatN n = take n . repeat

foldlSY :: (a->b->a) -> a -> Signal b -> a
foldlSY f z NullS     = z 
foldlSY f z (x:-xs) = foldlSY f (f z x) xs 
\end{code}



