{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.Example.SDF.ImageProcessing.Test
-- Copyright   :  (c) George Ungureanu, 2018
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  stable
-- Portability :  portable
--
-- Contains the testbench of this project.
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Example.SDF.ImageProcessing.Test where

import Data.List (intercalate)
import ForSyDe.Shallow (signal, fromSignal)
import ForSyDe.Shallow.Example.SDF.ImageProcessing.ImageProcessing
import ForSyDe.Shallow.Example.SDF.ImageProcessing.Utilities

-- | Testbench function for the ForSyDe process network
-- 'IL2212.ImageProcessing.imageProcessing' . It reads all the
-- <https://en.wikipedia.org/wiki/Netpbm_format P3 PPM images>
-- provided as list of file paths as a stream of pixels, feeds that
-- stream into the process network, collects the result stream and
-- prints out the output image(s).
--
-- __NOTE:__ all pictures need to have the same dimensions, since SDF
-- networks are static.
--
-- Example:
--
-- > testImageProcessing $ replicate 5 $ "files/test-imageproc/flag.ppm"
testImageProcessing :: [FilePath] -- ^ path to the input PPM files.
                    -> IO ()
testImageProcessing filePaths = do
  (dimX, dimY, imageStream) <- readAllPPM filePaths
  let -------testbench-------
      signalOut = imageProcessing dimX dimY (signal imageStream)
      -------testbench-------
      imageOut = intercalate "\n" $ partition dimX1 dimY1 $ fromSignal signalOut
      dimX1 = (dimX `div` 2) - 2
      dimY1 = (dimY `div` 2) - 2

  putStrLn imageOut
