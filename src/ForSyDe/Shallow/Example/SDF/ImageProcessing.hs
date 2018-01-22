{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.Example.SDF.ImageProcessing
-- Copyright   :  George Ungureanu
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This is a shallow toy image processing model used as system
-- specification for the <https://www.kth.se/student/kurser/kurs/IL2212?l=en KTH IL2212 Embedded Software>
-- lab project. It introduces the concept of parallel patterns as skeletons on vectors. 
--
-----------------------------------------------------------------------------

module ForSyDe.Shallow.Example.SDF.ImageProcessing (
  -- * Type aliases & data
  Image, Control, asciiLevels,
  -- * Functions on images
  grayscale, brightness, correction,
  resize, sobel, toAsciiArt,
  -- * SDF process network
  mooreSDF,
  imageProcessing,
  -- * Utilities
  -- ** Matrix skeletons

  -- | These skeletons (patterns of parallel computation and
  -- communication) are not defined in the @ForSyDe.Shallow@ library
  -- and are provided as local functions.
  mapMatrix, zipWithMatrix, reduceMatrix, rotateMatrix,
  lengthX, lengthY,

  -- ** Image utilities

  -- | Collection of functions for reading, printing or converting
  -- between image formats.
  toImage, fromImage, wrapImageF, printImage,
  readPPM, readAllPPM, ppm2img, partition,

  -- * Test bench function
    testImageProcessing
  ) where

import ForSyDe.Shallow.Example.SDF.ImageProcessing.ImageProcessing
import ForSyDe.Shallow.Example.SDF.ImageProcessing.Utilities
import ForSyDe.Shallow.Example.SDF.ImageProcessing.Test
