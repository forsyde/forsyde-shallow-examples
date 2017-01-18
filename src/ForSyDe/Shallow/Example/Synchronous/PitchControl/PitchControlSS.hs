{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  PitchControlSS
-- Copyright   :  Daniel Mauricio Muñoz Arboleda
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  damuz@unb.br
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module ForSyDe.Shallow.Example.Synchronous.PitchControl.PitchControlSS where

import ForSyDe.Shallow

type Sample = Double

-- | State space model A matrix declaration.
aMatrix = vector [vector [-0.313, 56.7, 0],
                  vector [-0.0139, -0.426, 0],
                  vector [0, 56.7, 0]]

-- | State space model B matrix declaration.
bMatrix = vector [0.232, 0.0203, 0]

-- | State space model C matrix declaration.
cMatrix = vector [0, 0, 1]

-- | State space model D matrix declaration.
dMatrix = 0.0

-- | Timestep.
deltaT = 0.1

-- declaration of processes
scalar_by_vector v scalar = mapV (*scalar) v

matrix_by_vector matrix v = sumV (zipWithV (*) (atV matrix 0) v)
                         :> sumV (zipWithV (*) (atV matrix 1) v)
                         :> sumV (zipWithV (*) (atV matrix 2) v)
                         :> NullV

vector_by_vector v1 v2 = sumV (zipWithV (*) v1 v2)
sumV = foldlV (+) 0.0

scalar_by_scalar scalar1 scalar2 = mapSY (*scalar1) scalar2

bProcess :: Signal Sample -> Signal (Vector Sample) 
bProcess = mapSY (scalar_by_vector bMatrix)

aProcess :: Signal (Vector Sample) -> Signal (Vector Sample)
aProcess = mapSY (matrix_by_vector aMatrix) 

cProcess :: Signal (Vector Sample) -> Signal Sample
cProcess = mapSY (vector_by_vector cMatrix)

dProcess :: Signal Sample -> Signal Sample
dProcess = scalar_by_scalar dMatrix

-- main process
pitchControlSS :: Signal Sample -> Signal Sample
pitchControlSS pidSignal = controlSignal
  where
    dotState = addSYVector outB outA
    outA = aProcess state
    outB = bProcess pidSignal
    controlSignal = addSample outD outC
    outC = cProcess state
    outD = dProcess pidSignal
    hatState = euler $ dotState
    state = integrator $ hatState
    
addSample :: Signal Sample -> Signal Sample -> Signal Sample
addSample a b = zipWithSY (+) a b

integrator = scanldSY vectorPlus (copyV 3 0.0)
vectorPlus a b = zipWithV (+) a b

addSYVector :: Signal (Vector Sample) -> Signal (Vector Sample) -> Signal (Vector Sample)
addSYVector v1 v2 = zipWithSY(zipWithV (+)) v1 v2

euler v =  mapSY(mapV (*0.1)) v
