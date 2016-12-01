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
-- This is the solution to Exercise 3.1 of Axel's book Modeling Embedded Systems
-- and Soc's.
-- 
-----------------------------------------------------------------------------

module ForSyDe.Examples.Untimed.Ascii2Bin (char2bin,
                                          bin2char) where

import ForSyDe.Shallow
import Data.Char

-- | 'char2bin' is a process that translates characters of the English alphabet
-- and digits into their binary ASCII representation. It reverts the operation
-- made by 'bin2char'.
--
-- For each firing, takes one token and generates 8 tokens.
--
-- >>> let sig = signal "A3b1"
-- >>> char2bin sig
-- {'0','1','0','0','0','0','0','1','0','0','1','1','0','0','1','1','0','1','1','0','0','0','1','0','0','0','1','1','0','0','0','1'}
char2bin :: Signal Char -- ^ Input signal
         -> Signal Char -- ^ Output signal
char2bin = mapU 1 char2bin'

-- | Auxiliary function to convert Chars into Strings of '0' and '1' 
char2bin' :: [Char] -> [Char]
char2bin' []     = []
char2bin' (x:xs) = (reverse . take 8 . int2bin $ ord x) ++ (char2bin' xs)

-- | Auxiliary function that generates an infinite list of chars representing
-- an integer in binary format with little endian codification.
int2bin :: Int -> [Char]
int2bin x   
    | mod x 2 == 0  = '0' : int2bin (div x 2)
    | otherwise     = '1' : int2bin (div (x - 1) 2)

-- | 'bin2char is a process that translates characters in binary form back to
-- an ascii representation. It reverts the operation made by 'char2bin'.
--
-- For each firing, takes 8 tokens and generates 1 token.
--
-- >>> let sig1 = signal "A3b1"
-- >>> let sig2 = bin2char sig1
-- >>> char2bin sig2
-- {'A','3','b','1'}
bin2char :: Signal Char -- ^ Input signal
         -> Signal Char -- ^ Output signal
bin2char = mapU 8 int2char

-- | Auxiliary function that converts strings of '0' and '1' back to
-- ascii representation.
int2char :: [Char] -> [Char]
int2char [] = []
int2char x  = (chr $ bin2int $ take 8 x) : (int2char $ drop 8 x)

-- | Auxiliary function that parses a string of '0' and '1' and converts it to
-- Int.
bin2int :: [Char] -> Int
bin2int x = sum $ zipWith (*) string powers2
    where   powers2 = reverse $ map (2^) [0..7]
            string = map (digitToInt) x



