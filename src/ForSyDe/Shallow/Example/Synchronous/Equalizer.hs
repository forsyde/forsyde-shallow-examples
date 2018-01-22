{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.Example.Synchronous.Equalizer
-- Copyright   :  Ingo Sander
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This is a shallow implementation of an equalizer system, used in
-- <http://urn.kb.se/resolve?urn=urn%3Anbn%3Ase%3Akth%3Adiva-3525 Ingo Sander's PhD thesis>.
-- The <files/docs/DocumentationEqualizer.pdf DOCUMENTATION> was
-- written in Literate Haskell style and generate with pdflatex.
--
-----------------------------------------------------------------------------

module ForSyDe.Shallow.Example.Synchronous.Equalizer (
  -- * Types used
  AnalyzerMsg, OverrideMsg, Sensor, Button,
  -- * ForSyDe processes
  equalizer,
  buttonControl,
  audioFilter,
  audioAnalyzer,
  distortionControl,
  -- * Testbench functions
  module ForSyDe.Shallow.Example.Synchronous.Equalizer.Test
  )where

import ForSyDe.Shallow.Example.Synchronous.Equalizer.AudioAnalyzer
import ForSyDe.Shallow.Example.Synchronous.Equalizer.AudioFilter
import ForSyDe.Shallow.Example.Synchronous.Equalizer.ButtonControl
import ForSyDe.Shallow.Example.Synchronous.Equalizer.DistortionControl
import ForSyDe.Shallow.Example.Synchronous.Equalizer.Equalizer
import ForSyDe.Shallow.Example.Synchronous.Equalizer.EqualizerTypes
import ForSyDe.Shallow.Example.Synchronous.Equalizer.Test
