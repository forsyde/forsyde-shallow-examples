\documentclass{article}

\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      commentstyle=\small\itfamily,
      keywordstyle=\small\bfseries,
      %identifierstyle=\small\underbar,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1
    }

\title{Pitch Control}
\author{daniel ingo dloubach}


\begin{document}
  \maketitle

  \section{Overview}
\begin{code}
module PitchControl where

import ForSyDe.Shallow

type Sample = Double

pitchControl :: Signal Sample -> Signal Sample
pitchControl setPoint = controlSignal
  where
    controlSignal = plant pidOutput
    pidOutput = pid (kp, ki, kd) errorSignal
    errorSignal = sigma setPoint measuredSignal
    measuredSignal = sensor controlSignal
    kp = 0.5021
    kd = 0.22
    ki = 0.1198

-- sensor has delay to break the 0 delay in the sensor feedback loop
sensor = delaySY 0.0

plant :: Signal Sample -> Signal Sample
plant = mapSY id

pid :: (Sample, Sample, Sample) -> Signal Sample -> Signal Sample
pid (kp, ki, kd) as = mealySY nextState outputDecoder s0 as
  where
    nextState state _ = state
    outputDecoder state input = kp * input + ki * (input + state) + kd * (input - state)
    s0 = 0.0

sigma :: Signal Sample -> Signal Sample -> Signal Sample
sigma as bs = zipWithSY (-) as bs


ctOutput = d2aConverter DAlinear 0.1 $ pitchControl $ takeS 50 ones
  where ones = 1.0:-ones

plotCtOutput = plotCT' 5e-4 [(ctOutput, "ctOutput")]


\end{code}

\end{document}
