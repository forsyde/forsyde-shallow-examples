\chapter{The module \haskell{Transceiver}}

\begin{figure}[htb!]
\centering%
\resizebox{!}{0.7\columnwidth}{\includegraphics{fig/Transceiver.eps}}
\caption{The structure of the Transceiver module}
\label{fig:Transceiver}
\end{figure}

The transceiver contains a receiver and a sender. The receiver receives an ASK-signal of the continuous time domain and outputs a signal of bitvectors that is modeled in the untimed domain. The sender conducts the opposite operation, but in addition, it also adapts the gain based on the input of a control signal.
\begin{code}
{-# OPTIONS_HADDOCK hide #-}
module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Transceiver
    where 

-- import ForSyDeMoCLib
-- import CTLib
-- import BitVector
-- import FilterLib

import ForSyDe.Shallow
import ForSyDe.Shallow.CTLib
import ForSyDe.Shallow.BitVector
import ForSyDe.Shallow.FilterLib

import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Parameters
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Utilities

transceiver :: -- (Fractional a) =>
               Signal (SubsigCT Double) -- Input: ASK Signal (to receiver) 
             -> Signal (Vector Integer) -- Input: Signal of bitvectors 
                                        --        (to sender)
             -> Signal ((Rational,Rational),
                        (Rational -> Double) 
                        -> Rational -> Double) 
                                       -- Input: Signal of analog functions 
                                       -- (to sender)
            -> Signal Double           -- Adaptive threshold value
            -> (Signal (Vector Integer),
                        Signal (SubsigCT Double),
                        Signal (SubsigCT Double))
                                       -- Output: (Signal of bitvectors
                                       --         (from receiver),
                                       --          ASK Signal (from sender))
transceiver sig_ct_waveReceived sig_sdf_Tx sig_ct_powerMode sig_sdf_thresh' =
                                    (sig_sdf_Rx, sig_ct_lpout, sig_ct_waveSent)
    where (sig_sdf_Rx,sig_ct_lpout) = receiver sig_ct_waveReceived sig_sdf_thresh'
          sig_ct_waveSent = sender sig_ct_powerMode sig_sdf_Tx

receiver :: -- (Fractional a) => 
             Signal (SubsigCT Double)         -- Input: ASK Signal
         ->  Signal Double                    -- Adaptive threshold value
         -> (Signal (Vector Integer),Signal (SubsigCT Double)) -- Output
receiver sig_ct_waveReceived sig_sdf_thresh' = (sig_sdf_Rx,sig_ct_lpout)
    where 
      (sig_sdf_Rx, sig_ct_lpout) = 
            (packBits 9 $ snd $ decodeASK sig_ct_waveReceived sig_sdf_thresh', 
             fst $ decodeASK sig_ct_waveReceived sig_sdf_thresh')

sender :: -- (Fractional a) => 
          Signal ((Rational,Rational),(Rational -> Double) -> Rational -> Double) 
                                  -- Input: Signal of analog functions 
       -> Signal (Vector Integer) -- Input: Signal of bitvectors 
       -> Signal (SubsigCT Double)         -- Output: ASK Signal 
sender sig_ct_powerMode sig_sdf_Tx = sig_ct_waveSent 
    where sig_ct_waveSent = adaptGain sig_ct_powerMode sig_ct_wave
          sig_ct_wave = -- mapSY toRational $ 
                 encodeASK period_bit radian_sin timeInterval sig_sdf_bit
          sig_sdf_bit = mapU 1 (fromVector . strip) sig_sdf_Tx
\end{code}
The process \texttt{adaptGain} is an adaptive process that amplifies an input signal according to the gain signal.
\begin{code}
adaptGain :: -- (Fractional a) =>
          Signal ((Rational,Rational),(Rational -> Double) -> Rational -> Double) 
                                      -- Input: Signal of analog functions  
          -> Signal (SubsigCT Double) -- Input: Analog signal
          -> Signal (SubsigCT Double) -- Output: Analog signal
adaptGain gainSignal ctSignal = pGainApplyfCT gainSignal ctSignal

pGainApplyfCT :: -- (Fractional a) => 
                  Signal ((Rational, Rational),
                          (Rational -> Double) -> Rational -> Double)
               -> Signal (SubsigCT Double)
               -> Signal (SubsigCT Double)
pGainApplyfCT = applyfCT sync2CTClockTime -- plotStepSize
\end{code}

The process \haskell{encodeASK} takes a signal of bits as input and two parameters, which describe the length of the period in seconds for one bit and the frequency of the sine carrier wave in Hertz as input and produces the ASK-signal for the given time interval.
\begin{code}
encodeASK period_bit freq_sin timeInterval sig_sdf_bit = sig_ct_wave
    where
        sig_ct_wave = modulateASK sig_ct_sine sig_ct_convBit
        sig_ct_convBit =
          d2aConverter DAhold period_bit (mapSY fromInteger sig_sdf_bit)
        sig_ct_sine = generateSineWave freq_sin timeInterval

generateSineWave freq_sin timeInterval 
    = signal [SubsigCT (sineFunction, timeInterval)]
      where
         sineFunction x = sin (fromRational $ x * freq_sin)

modulateASK sig_ct_sine sig_ct_convBit = multCT sig_ct_sine sig_ct_convBit
\end{code}

The process \texttt{decodeASK} takes a continuous time input signal and converts it to a signal of bits. It consists of two parts. The process \texttt{a2dConverter} converts the analog signal into a digital signal, where a single bit is represented by \texttt{samplesPerBit} values. The process \texttt{detectBitLevel} calculates the level of one bit out of \texttt{samplesPerBit} values.  
\begin{code}

decodeASK :: -- (Fractional a) =>
             Signal (SubsigCT Double) -- Input signal (continuous time) 
          ->  Signal Double                    -- Adaptive threshold value
          -> (Signal (SubsigCT Double),Signal Integer)  -- Output signal 
decodeASK sig_ct_waveReceived sig_sdf_thresh' = 
    (sig_ct_lpout,zipWithU 1 samplesPerBit detectBitLevel sig_sdf_thresh 
         $ a2dConverter (a2dResolution)
         $ sig_ct_lpout)
  where
    sig_ct_lpout = sLinearFilter RK4 a2dResolution' [1] filterDemCoef $ 
                                 absCT sig_ct_waveReceived
    sig_sdf_thresh = comb_sdf_adaptThresh sig_sdf_thresh'

comb_sdf_adaptThresh = combU 1 adaptThreshF
  where
    adaptThreshF [x] = repeatN numberOfBits x
\end{code}

