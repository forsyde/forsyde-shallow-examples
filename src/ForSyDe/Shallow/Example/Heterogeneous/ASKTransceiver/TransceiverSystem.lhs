\chapter{The module \haskell{TransceiverSystem}}

\begin{figure}[htb!]
\centering%
\resizebox{\columnwidth}{!}{\includegraphics{fig/TransceiverSystem.eps}}
\caption{The Structure of the module \haskell{TransceiverSystem}}
\label{fig:ASK_module}
\end{figure}

The transceiver system contains the Transceiver, Encryption, Decryption, Controller and interfaces between different model of computation domains. It is the system module under test. 

\begin{code}
{-# OPTIONS_HADDOCK hide #-}
module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.TransceiverSystem
    where 

-- import ForSyDeMoCLib
-- import CTLib
-- import BitVector

import ForSyDe.Shallow
import ForSyDe.Shallow.Utility.BitVector
import ForSyDe.Shallow.MoC.CT

import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Parameters
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Utilities

import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Transceiver
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.EncDec
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Controller


transceiverSystem ::
  Signal (SubsigCT Double) -- The input CT signal to ASK receiver
  -> Signal (Vector Integer) -- The input SDF signal to Encyption module
  -> Signal Integer     -- The input SR signal to control the Enc/Dec algorithms
  -> (Signal (Vector Integer),
      Signal (SubsigCT Double),
      Signal (SubsigCT Double),
      Signal (Vector Integer),
      Signal (Vector Integer),
      Signal Integer
--      , Signal Double
     )
transceiverSystem sig_ct_waveReceived sig_sr_testIn sig_sr_testCryptoMode = 
  (sig_sr_testOut, sig_ct_waveSent, sig_ct_lpout, sig_sr_Rx, 
                   sig_sdf_Tx, sig_sr_bitError
--                   , 
  )
  where
    -- Transceiver module
    (sig_sdf_Rx, sig_ct_lpout, sig_ct_waveSent) = 
            transceiver sig_ct_waveReceived sig_sdf_Tx sig_ct_powerMode sig_sdf_thresh'
    -- Encryption/decryption module
    (sig_sr_bitError, sig_sr_testOut, sig_sr_Tx)
         = moduleEncDec sig_sr_Rx sig_sr_testIn sig_sr_cryptoMode
    -- Controller module
    (sig_sr_cryptoMode, sig_sr_powerMode,sig_sdf_thresh')
         = moduleController sig_sr_bitError sig_sr_testCryptoMode
    -- interfaces
    sig_ct_powerMode = sync2CTInterface sync2CTClockTime sig_sr_powerMode
    sig_sr_Rx = sig_sdf_Rx
    sig_sdf_Tx = sig_sr_Tx
\end{code}
