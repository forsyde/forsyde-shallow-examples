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

-- | Clock data type.
type Clock = Int
-- | Internal register takes care of the actual state and how many
-- tokens have arrived.
type Register = (State, Count)
-- | To keep track of how many tokens have arrived since last state transition.
type Count = Int
-- | FSM states
data State = RR1 -- ^ Both signals red.
           | RY1 -- ^ Red / Yellow.
           | RG  -- ^ Red / Green.
           | RY2 -- ^ Red / Yellow.
           | RR2 -- ^ Both signals red.
           | YR1 -- ^ Yellow / Red.
           | GR  -- ^ Green / Red.
           | YR2 -- ^ Yellow / Red.

-- | Possible outputs.        
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
nsf :: Register                 -- ^ Actual state
    -> Clock                    -- ^ Input token
    -> Register                 -- ^ Next state
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

-- | Output function. This is a Moore machine, so the output depends
-- only on the current state.
outf :: Register                -- ^ State
     -> (Color, Color)          -- ^ Output signal
outf (RR1, cnt) = (RED, RED)
outf (RR2, cnt) = (RED, RED)
outf (RY1, cnt) = (RED, YELLOW)
outf (RY2, cnt) = (RED, YELLOW)
outf (RG, cnt)  = (RED, GREEN)
outf (YR1, cnt) = (YELLOW, RED)
outf (YR2, cnt) = (YELLOW, RED)
outf (GR, cnt)  = (GREEN, RED)



