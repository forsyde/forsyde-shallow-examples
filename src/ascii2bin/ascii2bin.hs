-----------------------------------------------------------------------------
-- |
-- Module      :  Ascii2bin
-- Copyright   :  José Edil Guimarães de Medeiros
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  j.edil@ene.unb.br
-- Stability   :  experimental
-- Portability :  portable
--
-- This is the solution to Exercise 3.1 of Axel's book using the untimed MoC.
-- 
-----------------------------------------------------------------------------

module Ascii2bin where

import ForSyDe.Shallow
import Data.Char

-- p is a process that translates characters of the English alphabet and digits
-- into their binary ASCII representation.
-- For each firing, takes one token and generates 8 tokens.
p :: Signal Char -> Signal Char
p = mapU 1 char2bin

char2bin :: [Char] -> [Char]
char2bin []     = []
char2bin (x:xs) = (reverse . take 8 . int2bin $ ord x) ++ (char2bin xs)

-- Generates an infinite list of chars with little endian codification.
int2bin :: Int -> [Char]
int2bin x   
    | mod x 2 == 0  = '0' : int2bin (div x 2)
    | otherwise     = '1' : int2bin (div (x - 1) 2)

-- q is a process that reverts the operation made by p.
-- For each firing, takes 8 tokens and generates 1 token.
q :: Signal Char -> Signal Char
q = mapU 8 int2char

int2char :: [Char] -> [Char]
int2char [] = []
int2char x  = (chr $ bin2int $ take 8 x) : (int2char $ drop 8 x)

bin2int :: [Char] -> Int
bin2int x = sum $ zipWith (*) string powers2
    where   powers2 = reverse $ map (2^) [0..7]
            string = map (digitToInt) x



