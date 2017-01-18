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
-- Transceiver used as case study in the <DocumentationTransceiver.pdf>
--
-----------------------------------------------------------------------------


module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver (
  module ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Main
  ) where



import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Gaussian
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Parameters
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Transceiver
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Utilities
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.Controller
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.EncDec
import ForSyDe.Shallow.Example.Heterogeneous.ASKTransceiver.TransceiverSystem
