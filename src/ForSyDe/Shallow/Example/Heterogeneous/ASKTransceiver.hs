-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  Ingo Sander
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ingo@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This is a shallow implementation of an Asynchronous Shift Key
-- Transceiver. The <files/DocumentationTransceiver.pdf DOCUMENTATION>
-- was written in Literate Haskell style and generate with pdflatex.
--
-----------------------------------------------------------------------------


 module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver (
   -- * Main
   module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Main,
   -- * Gaussian
   module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Gaussian,
   -- * Parameters
   module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Parameters,
   -- * Transceiver
   module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Transceiver,
   -- * Utilities
   module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Utilities,
   -- * Controller
   module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Controller,
   -- * EncDec
   module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.EncDec,
   -- * TransceiverSystem
   module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.TransceiverSystem
   ) where


import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Main
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Gaussian
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Parameters
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Transceiver
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Utilities
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Controller
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.EncDec
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.TransceiverSystem
