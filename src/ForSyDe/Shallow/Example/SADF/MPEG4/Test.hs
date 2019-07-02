module ForSyDe.Shallow.Example.SADF.MPEG4.Test where

import ForSyDe.Shallow hiding (Matrix, matrix)
import Data.Matrix
import ForSyDe.Shallow.Example.SADF.MPEG4.MPEG4

----------------------------------------------------------
--             MPEG4 SP Decoder parameters
----------------------------------------------------------

fs = (16,16)                      -- Frame size. Each element of fs should be a multiple of bs
bs = 8                            -- Macro blocks of bs x bs pixels
nb = div (uncurry (*) fs) (bs^2)  -- Number of macro blocks in a frame


----------------------------------------------------------
--                       Tests
----------------------------------------------------------

-- Before testing, remember to ignore the IDCT

frame1 = uncurry matrix fs $ \(i,j) -> 2*i + j
frame2 = identity (fst fs)
frame3 = uncurry fromList fs (repeat 1)

block1 = matrix bs bs $ \(i,j) -> (j-1)

iblocks = frame2mblocks (1,1) (bs,bs) frame2
p1block = FullB {pos = (3,3), block = block1, motionV = (0,-1)}
p2block1 = FullB {pos = (3,1), block = block1, motionV = (0,1)}
p2block2 = FullB {pos = (3,3), block = block1, motionV = (1,1)}

mvec = MotionVec { mvPos = (1, 1), mvVec = (-1, 0) }
mvecs = [mvec, mvec, mvec]

testFT = signal ["I", "P1", "P2", "P3"]
testinp = signal (iblocks ++ [p1block, p2block1, p2block2])

testout = pnMPEG4  bs nb fs testFT testinp

testMPEG = do
  putStrLn "\nMPEG test input blocks:\n"
  putStrLn "FT = "
  print testFT
  putStrLn "\nInput stream = "
  print testinp
  putStrLn "\nOutput frames:\n"
  print $ dropS 1 testout
  
