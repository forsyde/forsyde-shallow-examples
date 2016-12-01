-----------------------------------------------------------------------------
-- |
-- Module      :  TrafficLight
-- Copyright   :  José Edil Guimarães de Medeiros
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  j.edil@ene.unb.br
-- Stability   :  experimental
-- Portability :  portable
--
-- This is the traffic light controller module, example 4.8 on Axel's book.
-- 
-----------------------------------------------------------------------------

module ForSyDe.Examples.Synchronous.TrafficLight where

import ForSyDe.Shallow


type Clock = Int
type Register = (State, Count)
type Count = Int
data State = RR1 | RY1 | RG | RY2 | RR2 | YR1 | GR | YR2
data Color = RED | YELLOW | GREEN
    deriving Show

-- | 'trafficController' models a FSM for the traffic light
-- controller.  The Clock signal receives a sequence of tokens, each
-- representing a single clock tick.
--
-- >>> let sig = signal [1..20]
-- >>> trafficController sig
-- {(RED,RED),(RED,RED),(RED,YELLOW),(RED,YELLOW),(RED,YELLOW),(RED,YELLOW),
-- (RED,GREEN),(RED,GREEN),(RED,GREEN),(RED,GREEN),(RED,GREEN),(RED,GREEN),
-- (RED,GREEN),(RED,GREEN),(RED,GREEN),(RED,GREEN),(RED,GREEN),(RED,GREEN),
-- (RED,GREEN),(RED,GREEN),(RED,GREEN)}
trafficController :: Signal Int            -- ^ Clock signal
                  -> Signal (Color, Color) -- ^ Output signal
trafficController = mooreSY nsf outf (RR1, 0)

-- | Next state function. Implements the state diagram transitions.
nsf :: Register -> Clock -> Register
nsf (RR1, cnt) c    | cnt < 1 = (RR1, cnt+1)
                    | otherwise = (RY1, 0)
nsf (RY1, cnt) c    | cnt < 3 = (RY1, cnt+1)
                    | otherwise = (RG, 0)
nsf (RG, cnt)  c    | cnt < 60 = (RG, cnt+1)
                    | otherwise = (RY2, 0)
nsf (RY2, cnt) c    | cnt < 3 = (RY2, cnt+1)
                    | otherwise = (RR2, 0)
nsf (RR2, cnt) c    | cnt < 1 = (RR2, cnt+1)
                    | otherwise = (YR1, 0)
nsf (YR1, cnt) c    | cnt < 3 = (YR1, cnt+1)
                    | otherwise = (GR, 0)
nsf (GR, cnt)  c    | cnt < 60 = (GR, cnt+1)
                    | otherwise = (YR2, 0)
nsf (YR2, cnt) c    | cnt < 3 = (YR2, cnt+1)
                    | otherwise = (RR1, 0)

-- | Output function.
outf :: Register -> (Color, Color)
outf (RR1, cnt) = (RED, RED)
outf (RR2, cnt) = (RED, RED)
outf (RY1, cnt) = (RED, YELLOW)
outf (RY2, cnt) = (RED, YELLOW)
outf (RG, cnt)  = (RED, GREEN)
outf (YR1, cnt) = (YELLOW, RED)
outf (YR2, cnt) = (YELLOW, RED)
outf (GR, cnt)  = (GREEN, RED)



