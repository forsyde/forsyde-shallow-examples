{-# OPTIONS_HADDOCK not-home #-}

module ForSyDe.Shallow.Example.Synchronous.Equalizer (
  -- * Types used
  AnalyzerMsg, OverrideMsg, Sensor, Button,
  -- * ForSyDe processes
  equalizer,
  buttonControl,
  audioFilter,
  audioAnalyzer,
  distortionControl,
  -- * Testbench module
  module ForSyDe.Shallow.Example.Synchronous.Equalizer.Test
  )where

import ForSyDe.Shallow.Example.Synchronous.Equalizer.AudioAnalyzer
import ForSyDe.Shallow.Example.Synchronous.Equalizer.AudioFilter
import ForSyDe.Shallow.Example.Synchronous.Equalizer.ButtonControl
import ForSyDe.Shallow.Example.Synchronous.Equalizer.DistortionControl
import ForSyDe.Shallow.Example.Synchronous.Equalizer.Equalizer
import ForSyDe.Shallow.Example.Synchronous.Equalizer.EqualizerTypes
import ForSyDe.Shallow.Example.Synchronous.Equalizer.Test
