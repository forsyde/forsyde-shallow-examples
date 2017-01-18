\chapter{The module \haskell{Encryption and Decryption}}

\begin{figure}[htb!]
\centering%
\includegraphics{fig/EncDec.eps}
\caption{The Structure of the Encryption and Decryption module}
\label{fig:EncDec_module}
\end{figure}

The main module for encryption and decryption module in SDF domain.

\begin{code}
{-# OPTIONS_HADDOCK hide #-}
module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.EncDec 
    where 

-- import ForSyDeMoCLib
-- import AdaptivityLib
-- import CTLib
-- import BitVector

import ForSyDe.Shallow
import ForSyDe.Shallow.CTLib
import ForSyDe.Shallow.AdaptivityLib
import ForSyDe.Shallow.BitVector

import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Utilities

moduleEncDec sig_sr_Rx sig_sr_testIn sig_sr_cryptoMode
                  = (sig_sr_bitError, sig_sr_testOut, sig_sr_Tx)
  where
    sig_sr_bitError = detectBitErrors sig_sr_Rx
    sig_sr_testOut = adaptiveDecoder sig_sr_Rx
    sig_sr_Tx = adaptiveEncoder sig_sr_testIn
    -- Processes used
    adaptiveDecoder = pAdaptiveSR (adaptiveDecController sig_sr_cryptoMode) 
                                                . pBitsStripper
    adaptiveEncoder = pEvenParityWrapper . pAdaptiveSR 
                                   (adaptiveEncController sig_sr_cryptoMode)
\end{code}

The processes used by this module. \\
The process \texttt{pBitsStripper} is to strip off the even parity 
bit from the 9-bit bitVector Signal.

\begin{code}
pBitsStripper :: Signal (Vector Integer) -> Signal (Vector Integer)
pBitsStripper =  mapSY removeParityBit 
-- pBitsStripper = combU 1 $ wrap . removeParityBit . strip
               
\end{code}

The process \texttt{pEvenParityWrapper} is to wrap an even parity bit 
in the head for the 8-bit bitVector Signal.

\begin{code}
pEvenParityWrapper :: Signal (Vector Integer) -> Signal (Vector Integer)
pEvenParityWrapper = mapSY $ addParityBit Even
-- pEvenParityWrapper = combU 1 $ wrap . addParityBit Even . strip 
\end{code}

The process \texttt{bitErrorRate} is to validation of the even parity bit 
in the head for the 9-bit bitVector Signal.

\begin{code}
detectBitErrors :: Signal (Vector Integer) -> Signal Integer
detectBitErrors = mapSY isEvenParity'
-- detectBitErrors = combU 1 $ wrap . isEvenParity' . strip  
  where
    isEvenParity' x | isEvenParity x = 0
                    | otherwise = 1
\end{code}

The  process \texttt{pAdaptiveSR} is the adaptive process, which applies the 
functions defined in the first signal on the second input signal.

\begin{code}
pAdaptiveSR :: Signal (a->b) -> Signal a -> Signal b
pAdaptiveSR = applyfSY

-- Some adaptive implementations:
--    adaptiveDecoder = pAdaptiveSDF decodingAlgorithm . pBitsStripper
--    adaptiveEncoder = pEvenParityWrapper . pAdaptiveSDF encodingAlgorithm
\end{code}


