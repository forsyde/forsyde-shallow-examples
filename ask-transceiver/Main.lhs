\chapter{The module \haskell{Main}}

\begin{figure}[htb!]
\centering%
\resizebox{\columnwidth}{!}{\includegraphics{fig/TestBench.eps}}
\caption{The Structure of the module \haskell{Main}}
\label{fig:TestBench_module}
\end{figure}

\begin{code}
--module Main 
--    where


import System.IO
import System.IO.Unsafe


-- import ForSyDeMoCLib
-- import CTLib
-- import FilterLib
-- import BitVector
import Gaussian


import ForSyDe.Shallow
import ForSyDe.Shallow.CTLib
import ForSyDe.Shallow.FilterLib
import ForSyDe.Shallow.BitVector



import Parameters
import Transceiver
import Utilities
import Controller
import EncDec
import TransceiverSystem



\end{code}

The main function for TestBench module in CT, synchronous and SDF domain.\\
Type ``main'' in GHCI to run the test.

\begin{code}

main = do
   putStrLn "Testing ..."
   putStrLn "Input signal of integers"
   putStrLn ("input = " ++ (show $ integerInput))
   putStrLn "Output signal of integers"
   putStrLn ("output = " ++ (show $ integerOutput))

   plotCT' plotStepSize [(subCT sig_ct_waveReceived sig_ct_waveSentAttenuated, "sig_ct_noise")] 
   plotCT' plotStepSize [(sig_ct_waveSent, "sig_ct_waveSent")] 
   plotCT' plotStepSize [(sig_ct_waveReceived, "sig_ct_waveReceived")] 
   plotCT' plotStepSize [(sig_ct_lpout, "sig_ct_lpout")] 
   putStrLn "Done!"   

{-
-}
sig_sdf_lpin = a2dConverter dataPeriod sig_ct_waveReceived
sig_sdf_lpout = a2dConverter dataPeriod sig_ct_lpout

\end{code}
To test the transceiver system.
\begin{code}

integerInput = mapSY bitVectorToInt sig_sr_testIn
integerOutput = mapSY bitVectorToInt sig_sr_testOut

(sig_sr_testOut, sig_ct_waveSent, sig_ct_lpout,
 sig_sr_Rx, sig_sdf_Tx, sig_sr_bitError) = 
       transceiverSystem sig_ct_waveReceived sig_sr_testIn sig_sr_testCryptoMode
\end{code}

Sub-module 1 of the testbench is just to generate the stimuli signal which will
 be sent into the Encryption module. It is in SDF domain.
\begin{code}

sig_sr_testIn = mapSY (intToBitVector 8) $ signal integerList

\end{code}
Sub-module 2 of the testbench is to initialize the signal to control the enc-dec
 algorithms. It is in synchronous domain.
\begin{code}
sig_sr_testCryptoMode :: Signal Integer
sig_sr_testCryptoMode = signal (zeros3++ones5++zeros2)
  where
    zeros2 = take 2 $ repeat 0
    zeros3 = take 3 $ repeat 0    
    ones5 = take 5 $ repeat 1 
\end{code}
Sub-module 3 of the testbench is to add Gaussian noise into the ouput from the 
ASK Sender. The new signal with noise will be sent back into the ASK receiver.
The `sigVarivance' with possible variable variances is used to generate the
gaussian noise.
\begin{code}
sig_ct_waveReceived = addCT sig_ct_waveSentAttenuated sig_ct_noise
-- Here is the signal called 'noisy_signal' in SystemC group.
sig_sdf_waveReceived = a2dConverter a2dResolution'  sig_ct_waveReceived

-- To attenuate the ouput signal from ASK
sig_ct_waveSentAttenuated = scaleCT attenuation sig_ct_waveSent
-- The gaussian noise
sig_ct_noise = d2aConverter DAlinear a2dResolution' sig_GaussianDouble2 -- gaussianNoise 
--gaussianNoise =  gaussianNoiseGen 0.0 sigVarivance myRandDouble0
sig_GaussianDouble2 = zipWithU 1 2 (gaussianF2 0 ) sigVarivance sig_myRand   

-- The variance of the gaussian noise
sigVarivance | change_variance = signal $ repeatN change_var_start gaussianVar
                                   ++ repeatN (change_var_end- change_var_start) 
                                              (gaussianVar*change_var_factor)
                                   ++ repeat gaussianVar
             | otherwise = signal $ repeat gaussianVar
\end{code}
