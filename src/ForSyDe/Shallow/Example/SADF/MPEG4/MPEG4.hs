-----------------------------------------------------------------------------
-- |
-- Module  :  MPEG4
-- Copyright   :  (c) Ricardo Bonna
-- License     :  still needs license
--
-- Maintainer  :  ricardobonna@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This is a prototype MPEG4 Simple Profile Decoder
--
-----------------------------------------------------------------------------

module ForSyDe.Shallow.Example.SADF.MPEG4.MPEG4 where

import ForSyDe.Shallow hiding (Matrix, matrix)
import Data.Matrix    -- use cabal install matrix to instal the matrix package

----------------------------------------------------------
--                 Datatypes definitions
----------------------------------------------------------

type Block = Matrix Int       -- dimentions = bs x bs
type Frame = Matrix Int       -- dimentions = fs

data MacroBlock = FullB { pos :: (Int, Int),
                          motionV :: (Int, Int),
                          block :: Block } |
                  PosB { pos :: (Int, Int),
                         block :: Block } deriving (Show)

data MotionVec = MotionVec { mvPos :: (Int, Int),
                             mvVec :: (Int, Int) }


----------------------------------------------------------
--             MPEG4 SP Decoder parameters
----------------------------------------------------------

--fs = (16,16)                      -- Frame size. Each element of fs should be a multiple of bs
--bs = 8                            -- Macro blocks of bs x bs pixels
--nb = div (uncurry (*) fs) (bs^2)  -- Number of macro blocks in a frame

----------------------------------------------------------
--                 Auxiliar functions
----------------------------------------------------------

-- Apply the Inverse Discrete Cosine Transform (IDCT) to a matrix of Ints
inverseDCT :: Block -> Block
inverseDCT x = fmap round y
  where y = transpose dct * fmap fromIntegral x * dct
        dct = matrixDCT (nrows x)


-- Creates a n x n matrix for the Discrete Cosine Transform (DCT)
matrixDCT :: (RealFloat a) => Int -> Matrix a
matrixDCT n = matrix n n (\(i,j) -> sqrt (2/fromIntegral n) * if i == 1 then 1 / sqrt 2
                else cos (fromIntegral ((2*j - 1)*(i-1)) * pi / (2 * fromIntegral n)))


-- Block adds matrix x to matrix y in a the position given by (row,col)
blockAdd :: MacroBlock -> Block -> Block
blockAdd b y = matrix (nrows y) (ncols y) (\(i,j) -> if notElem i rows || notElem j cols
                                    then y ! (i,j) else x ! (i-row+1,j-col+1) + y ! (i,j))
  where (row, col) = pos b
        x = block b
        rows = [row .. (min (nrows y) (row - 1 + nrows x))] -- rows range
        cols = [col .. (min (ncols y) (col - 1 + ncols x))] -- cols range


-- Split a large block into a list of macro blocks of size (dr,dc) or smaller
frame2mblocks :: (Int,Int) -> (Int,Int) -> Block -> [MacroBlock]
frame2mblocks (pr,pc) (dr,dc) x
  | dr >= nrows x && dc >= ncols x = [PosB {pos = (pr,pc), block = x}]
  | dr >= nrows x = PosB {pos = (pr,pc), block = submatrix 1 (nrows x) 1 dc x} :
                    frame2mblocks (pr, pc+dc) (dr,dc) (submatrix 1 (nrows x) (dc+1) (ncols x) x)
  | dc >= ncols x = PosB {pos = (pr,pc), block = submatrix 1 dr 1 (ncols x) x} :
                    frame2mblocks (pr+dr, pc) (dr,dc) (submatrix (dr+1) (nrows x) 1 (ncols x) x)
  | otherwise = b : frame2mblocks (pr, pc+dc) (dr,dc) m2 ++ frame2mblocks (pr+dr, pc) (dr,dc) m3
                    ++ frame2mblocks (pr+dr, pc+dc) (dr,dc) m4
  where (m1,m2,m3,m4) = splitBlocks dr dc x
        b = PosB {pos = (pr,pc), block = m1}


-- Gets a matrix x and a list of motion vectors and returns a motion compensated matrix
motionComp :: [MotionVec] -> Block -> Int -> Block
motionComp mvs x bs = out
  where y = frame2mblocks (1,1) (bs, bs) x
        z1 = [PosB {pos = (fst (pos a)+fst (mvVec b), snd (pos a)+snd (mvVec b)),
             block = block a} | a <- y, b <- mvs, pos a == mvPos b]
        z2 = [a | a <- y, notElem (pos a) (map (\MotionVec {mvPos = p} -> p) mvs)]
        out = foldr blockAdd (zero (nrows x) (ncols x)) (z1++z2)


-- Reconstruct frame based on a former frame x and a list of macro blocks mbs
frameRC :: [MacroBlock] -> Block -> Block
frameRC mbs x = foldr blockAdd x mbs


----------------------------------------------------------
--          Lists of scenarios for each kernel
----------------------------------------------------------

type VLDscenario = (Int, (Int,Int), [MacroBlock] -> ([MacroBlock], [MotionVec]))
type IDCTscenario = (Int, Int, [MacroBlock] -> [MacroBlock])
type MCscenario = ((Int,Int), Int, [MotionVec] -> [Block] -> [Block])
type RCscenario = ((Int,Int), (Int, Int), [MacroBlock] -> [Block] -> ([Block], [Bool]))


-- List of scenarios for the VLD Kernel
scenariosVLD :: Int -> VLDscenario
scenariosVLD n
  | n == 0 = (1, (1,0), \[mb] -> ([PosB {pos = pos mb, block = block mb}],[]))
  | n == 1 = (1, (1,1), \[FullB{pos = p, motionV = mv, block = b}] -> ([PosB {pos = p, block = b}], [MotionVec {mvPos = p, mvVec = mv}]))
  | otherwise = error "scenariosVLD: Out of scenario range"


-- List of scenarios for the IDCT Kernel
scenariosIDCT :: Int -> IDCTscenario
scenariosIDCT n
  | n == 1 = (1, 1, \[PosB{pos = p, block = b}] -> [PosB {pos = p, block = inverseDCT b}])
  | otherwise = error "scenariosIDCT: Out of scenario range"


-- List of scenarios for the MC Kernel
scenariosMC :: Int -> Int -> Int -> (Int, Int) -> MCscenario
scenariosMC 0 _ _ fs = ((0,1), 1, \_ _ -> [uncurry zero fs])
scenariosMC n bs nb fs
  | n > 0 && n < nb = ((n,1), 1, \b [m] -> [motionComp b m bs])
  | otherwise = error "scenariosMC: Out of scenario range"


-- List of scenarios for the RC Kernel
scenariosRC :: Int -> Int -> RCscenario
scenariosRC 0 nb = ((nb,1), (1,1), \a [b] -> ([frameRC a b], [True]))
scenariosRC n nb
  | n > 0 && n < nb = ((n,1), (1,1), \a [b] -> ([frameRC a b], [True]))
  | otherwise = error "scenariosRC: Out of scenario range"


----------------------------------------------------------
--  Lists of scenarios and next state function for FD
----------------------------------------------------------

type FDscenario = ((Int, Int, Int, Int), ([VLDscenario], [IDCTscenario], [MCscenario], [RCscenario]))
type FDstate = Int

-- List of scenarios for the FD Detector
scenariosFD :: Int -> Int -> (Int, Int) -> FDstate -> FDscenario
scenariosFD bs nb fs 0 = ((nb, nb, 1, 1), (replicate nb $ scenariosVLD 0, replicate nb $ scenariosIDCT 1, [scenariosMC 0 bs nb fs], [scenariosRC 0 nb]))
scenariosFD bs nb fs n
  | n > 0 && n < nb = ((n, n, 1, 1), (replicate n $ scenariosVLD 1, replicate n $ scenariosIDCT 1, [scenariosMC n bs nb fs], [scenariosRC n nb]))
  | otherwise = error "scenariosFD: Out of scenario range"

nextStateFD :: Int -> FDstate -> [String] -> [Bool] -> FDstate
nextStateFD _ _ ["I"] [True] = 0
nextStateFD nb _ ['P':x] [True]
  | n > 0 && n < nb = n
  where n = read x :: Int
nextStateFD _ _ _ _ = error "nextStateFD: Unknown input sequence"


----------------------------------------------------------
--          Kernels and detector definitions
----------------------------------------------------------

-- FD detector definition
detectorFD :: Int -> Int -> (Int, Int) -> Signal String -> Signal Bool
           -> (Signal VLDscenario, Signal IDCTscenario, Signal MCscenario, Signal RCscenario)
detectorFD bs nb fs = detector24SADF (1,1) (nextStateFD nb) (scenariosFD bs nb fs) 0

-- VLD kernel definition
kernelVLD :: Signal VLDscenario -> Signal MacroBlock -> (Signal MacroBlock, Signal MotionVec)
kernelVLD = kernel12SADF

-- IDCT kernel definition
kernelIDCT :: Signal IDCTscenario -> Signal MacroBlock -> Signal MacroBlock
kernelIDCT = kernel11SADF

-- MC kernel definition
kernelMC :: Signal MCscenario -> Signal MotionVec -> Signal Frame -> Signal Frame
kernelMC = kernel21SADF

-- RC kernel definition
kernelRC :: Signal RCscenario -> Signal MacroBlock -> Signal Frame -> (Signal Frame, Signal Bool)
kernelRC = kernel22SADF

----------------------------------------------------------
--            Process Network definition
----------------------------------------------------------


pnMPEG4 :: Int -> Int -> (Int, Int) -> Signal String -> Signal MacroBlock -> Signal Frame
pnMPEG4 bs nb fs sigft sigmb = sigout
  where (sigout, sigfb) = kernelRC ctRC sigidct sigpf
        sigidct = kernelIDCT ctIDCT sigdb
        (sigdb, sigv) = kernelVLD ctVLD sigmb
        sigpf = kernelMC ctMC sigv sigout'
        (ctVLD, ctIDCT, ctMC, ctRC) = detectorFD bs nb fs sigft sigfb'
        sigout' = delaySADF [uncurry zero fs] sigout
        sigfb' = delaySADF (replicate 3 True) sigfb
