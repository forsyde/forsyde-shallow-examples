\chapter{The module \haskell{Controller}}

\begin{figure}[htb!]
\centering%
\includegraphics{fig/Controller.eps}
\caption{The Structure of the Controller module}
\label{fig:Controller_module}
\end{figure}

The \haskell{Controller} is modelled in the synchronous domain. There is one input to control the encoding and decoding algorithms. The bit error rate input will be analysed by the adaptive power controller to set the gain, which is an input to the sender in the transceiver module.
\begin{code}
{-# OPTIONS_HADDOCK hide #-}
module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Controller 
    where 

-- import ForSyDeMoCLib
-- import CTLib

import ForSyDe.Shallow
import ForSyDe.Shallow.MoC.CT

import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Utilities
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Parameters

moduleController :: 
     Signal Integer
  -> Signal Integer
  -> (Signal Integer, 
      Signal ((Rational -> Double) -> Rational -> Double),
      Signal Double)
moduleController sig_sr_bitError sig_sr_testCryptoMode = 
     (sig_sr_cryptoMode, sig_sr_powerMode,sig_sdf_thresh)
  where
    -- Encoding/decoding mode signal
    sig_sr_cryptoMode = sig_sr_testCryptoMode    
    -- Power mode signal
    sig_sr_powerMode = adaptivePowerController ((signal [0])  +-+ sig_sr_bitError )
    -- Threshold mode signal
    sig_sdf_thresh = --  signal $ repeat threshVal
            adaptiveThresholdController ((signal [0])  +-+ sig_sr_bitError )

-- Some adaptive implementations:
--    Encoding function signal    
--     (Signal ([Vector Integer] -> [Vector Integer]),
--    encodingAlgorithm' = adaptiveEncController controllInput
--    Decoding function signal
--      Signal ([Vector Integer] -> [Vector Integer]),
--    decodingAlgorithm' = adaptiveDecController controllInput
\end{code}


