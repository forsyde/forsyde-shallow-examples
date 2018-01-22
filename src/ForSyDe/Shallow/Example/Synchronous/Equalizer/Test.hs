module ForSyDe.Shallow.Example.Synchronous.Equalizer.Test (
  -- * Test data
  -- ** Test signals
  zeros, ones, increm, modCos,
  bassUp, bassDn, trebleUp, trebleDn,
  overrides, distorsionFlags,
  -- ** Test filter coefficients
  lpCoeff, bpCoeff, hpCoeff,
  lpCoeffD, bpCoeffD, hpCoeffD,
  -- ** Test vectors
  testV1, testV2, testV3,
  -- * Test functions
  -- ** Signal processing blocks
  testDFT, testFFT, testDftAgainstFft, testFIR,
  audioFilterI, audioFilterD,
  -- ** Equalizer modules
  testAudioFilterD,
  testButtonControl,
  testDistortionControl,
  testAnalyzer,
  testEqualizer,
  -- * Test scripts 
  testFilterScript,
  testAnalyzerScript,
  testFilterAndAnalyzerScript 
  )where

import Data.Complex
import System.IO
import System.FilePath
import ForSyDe.Shallow
import ForSyDe.Shallow.Utility.FIR
import ForSyDe.Shallow.Example.Synchronous.Equalizer.AudioAnalyzer
import ForSyDe.Shallow.Example.Synchronous.Equalizer.AudioFilter
import ForSyDe.Shallow.Example.Synchronous.Equalizer.ButtonControl
import ForSyDe.Shallow.Example.Synchronous.Equalizer.DistortionControl
import ForSyDe.Shallow.Example.Synchronous.Equalizer.Equalizer
import ForSyDe.Shallow.Example.Synchronous.Equalizer.EqualizerTypes

---------------
-- TEST DATA -- 
---------------

-- | test signal for 'testButtonControl'
bassUp = signal [
  Prst Active, Prst Active, Abst, Abst,
  Prst Active, Prst Active, Abst, Abst,
  Prst Active, Prst Active, Abst, Abst,
  Prst Active, Prst Active, Abst, Abst ]

-- | test signal for 'testButtonControl'
bassDn = signal [
  Abst, Abst, Prst Active, Abst,
  Abst, Abst, Prst Active, Abst,
  Abst, Abst, Prst Active, Abst,
  Abst, Abst, Prst Active, Abst ]

-- | test signal for 'testButtonControl'
trebleUp = signal [
  Abst, Abst, Abst, Abst,
  Abst, Abst, Abst, Prst Active,
  Abst, Abst, Abst, Abst,
  Abst, Abst, Abst, Prst Active ]

-- | test signal for 'testButtonControl'
trebleDn = signal [
  Abst, Abst, Abst, Prst Active,
  Abst, Abst, Abst, Prst Active,
  Abst, Abst, Abst,  Prst Active,
  Abst, Abst, Abst, Prst Active ]

-- | test signal for 'testButtonControl'
overrides = signal [
  Abst, Abst, Abst, Abst,
  Prst Lock, Abst, Abst, Abst,
  Abst, Prst CutBass, Abst, Abst,
  Prst Release,  Abst, Abst, Abst ]

-- | example signal for testing
-- 'ForSyDe.Shallow.Example.Synchronous.Equalizer.DistorsionControl.distortionControl'
distorsionFlags = signal [
  Abst, Abst, Abst, Prst Fail,
  Abst, Abst, Abst, Prst Fail,
  Abst, Abst, Abst, Prst Fail,
  Abst, Abst, Abst, Prst Fail,
  Abst, Abst, Abst, Prst Pass,
  Abst, Abst, Abst, Prst Pass,
  Abst, Abst, Abst, Prst Pass,
  Abst, Abst, Abst, Prst Pass,
  Abst, Abst, Abst, Prst Fail,
  Abst, Abst, Abst, Prst Fail,
  Abst, Abst, Abst, Prst Pass,
  Abst, Abst, Abst, Prst Pass,
  Abst, Abst, Abst, Prst Pass,
  Abst, Abst, Abst, Prst Pass,
  Abst, Abst, Abst, Prst Fail,
  Abst, Abst, Abst, Prst Pass,
  Abst, Abst, Abst, Prst Fail,
  Abst, Abst, Abst, Prst Pass,
  Abst, Abst, Abst, Prst Pass ]        

-- | Low-pass filter coefficients
lpCoeff = vector [
  0.01392741661548,
  0.01396895728902,
  0.01399870011280,
  0.01401657422649,
  0.01402253700635,
  0.01401657422649,
  0.01399870011280,
  0.01396895728902,
  0.01392741661548 ]

-- | Low-pass filter coefficients          
lpCoeffD = vector [
   0.03898579822345,
   0.09739504968381,
   0.15360490491115,
   0.19416166962179,
   0.20893350067585,
   0.19416166962179,
   0.15360490491115,   
   0.09739504968381,
   0.03898579822345
   ]

-- | Band-pass filter coefficients
bpCoeff = vector [
  0.06318761339784,
  0.08131651217682,
  0.09562326700432,
  0.10478344432968,
  0.10793629404886,
  0.10478344432968,
  0.09562326700432,
  0.08131651217682,
  0.06318761339784 ]

-- | Band-pass filter coefficients
bpCoeffD = vector [
  -0.07845593083988,
   0.00000000000000,
  -0.30707118658796,
   0.00000000000000,
   0.58268794919522,
   0.00000000000000,
  -0.30707118658796,
   0.00000000000000,
  -0.07845593083988
   ]

-- | High-pass filter coefficients
hpCoeff = vector [
  -0.07883878579454,
  -0.09820015927379,
  -0.11354603917221,
  -0.12339860164118,
   0.87320570334018,
  -0.12339860164118,
  -0.11354603917221,
  -0.09820015927379,
  -0.07883878579454 ]

-- | High-pass filter coefficients
hpCoeffD = vector [
   0.03898579822345,  
  -0.09739504968381,   
   0.15360490491115,
  -0.19416166962179,
   0.20893350067585,
  -0.19416166962179,
   0.15360490491115,
  -0.09739504968381,
   0.03898579822345
  ]


-- | Infinite signal of zeroes
zeros = infiniteS id 0.0

-- | Finite signal of ones, having its length given as input
ones pts = takeS pts $ infiniteS id 1.0

-- | Incremental signal of 400 samples
increm = takeS 400 (infiniteS (+1/200) 0.0)
 
-- | Additive sinusoidal signal of 400 samples, with 3 sine components.
modCos = mapSY modCosF increm
  where
    modCosF x = cos10 x + cos50 x + cos90 x
    cos10 x = cos (2*pi*10*x)
    cos50 x = cos (2*pi*50*x)
    cos90 x = cos (2*pi*90*x)


-- | example input vector to test the ForSyDe implementation of FFT and DFT
testV1 = mapV toComplex (vector [1, 2])

-- | example input vector to test the ForSyDe implementation of FFT and DFT
testV2 = mapV toComplex (vector [1, 2,3,4])

-- | example input vector to test the ForSyDe implementation of FFT and DFT
testV3 = mapV toComplex (vector [1, 2, 3, 4, 5, 6, 7, 8])

--------------------
-- TEST FUNCTIONS -- 
--------------------
            
-- | tests the
-- 'ForSyDe.Shallow.Example.Synchronous.Equalizer.ButtonControl.buttonControl'
-- function.
testButtonControl = buttonControl overrides bassUp bassDn trebleUp trebleDn
--testButtonInterface = buttonInterface  bassUp bassDn trebleUp trebleDn 

toComplex a = a:+0

-- | tests ForSyDe's 'ForSyDe.Shallow.dft' against 'testV3'
testDFT = dft 8 testV3

-- | tests ForSyDe's 'ForSyDe.Shallow.fft' against 'testV3'
testFFT = fft 8 testV3

-- | tests both 'ForSyDe.Shallow.dft' and 'ForSyDe.Shallow.fft'
testDftAgainstFft = zipWithV (-) testDFT testFFT


-- | tests ForSyDe's 'ForSyDe.Shallow.firSY' with 4 coefficients
-- against an impulse signal.
testFIR = firSY coeff s
  where
    coeff = vector [0.1, -0.2, 0.5, 0.2]
    s = signal [1.0, 0, 0, 0, 0]

pts = 4

-- | tests the
-- 'ForSyDe.Shallow.Example.Synchronous.Equalizer.DistorsionControl.distortionControl'
-- function against 'distorsionFlags'.
testDistortionControl = distortionControl distorsionFlags

-- | Tests the
-- 'ForSyDe.Shallow.Example.Synchronous.Equalizer.AudioFilter.audioAnalyzer'
-- against a 'audioFilter'ed dummy signal.
testAnalyzer = audioAnalyzer 2 (audioFilter lpCoeff bpCoeff hpCoeff bass treble audioIn)
  where audioIn = ones (pts * 4)
        bass    = zeros
        treble  = zeros

-- | Instantiates
-- 'ForSyDe.Shallow.Example.Synchronous.Equalizer.AudioFilter.audioFilter'.
audioFilterI = audioFilter lpCoeff bpCoeff hpCoeff zeros zeros

-- | Instantiates
-- 'ForSyDe.Shallow.Example.Synchronous.Equalizer.AudioFilter.audioFilter'.
audioFilterD = audioFilter lpCoeffD bpCoeffD hpCoeffD zeros zeros

-- | Tests
-- 'ForSyDe.Shallow.Example.Synchronous.Equalizer.AudioFilter.audioFilter'
-- against a test signal.
testAudioFilterD = audioFilter lpCoeffD bpCoeffD hpCoeffD zeros zeros s
  where s = signal [0.1,0.05,0.3,0.2,-0.5,0.2,0.1,0.3,0.1,-0.1,0.0,0.2]
                  
-- | Tests the
-- 'ForSyDe.Shallow.Example.Synchronous.Equalizer.Equalizer.equalizer'
-- function against a dummy signal consisting of all ones.
testEqualizer = equalizer lpCoeff bpCoeff hpCoeff 2 bassUp bassDn trebleUp trebleDn audioIn
  where audioIn = ones (pts * 4)
        
--testButtonInterface = buttonInterface  bassUp bassDn trebleUp trebleDn 


-- fs = 200

--lpCoeffF8 = mapV real2Fixed8 lpCoeffD
--bpCoeffF8 = mapV real2Fixed8 bpCoeffD
--hpCoeffF8 = mapV real2Fixed8 hpCoeffD
--sF8    = mapSY real2Fixed8 s

--lpCoeffF16 = mapV real2Fixed16 lpCoeffD
--bpCoeffF16 = mapV real2Fixed16 bpCoeffD
--hpCoeffF16 = mapV real2Fixed16 hpCoeffD
--sF16    = mapSY real2Fixed16 s

--outF16    = audioFilter lpCoeffF16 bpCoeffF16 hpCoeffF16 (mapSY real2Fixed16 zeros) (mapSY real2Fixed16 zeros) sF16
--outF8    = audioFilter lpCoeffF8 bpCoeffF8 hpCoeffF8 (mapSY real2Fixed8 zeros)(mapSY real2Fixed8 zeros) sF8

--writeAudioOut = writeFile "Test/AudioOut.for") . writeS 



-------------
-- SCRIPTS --
-------------

-- | Tests the 'audioFilterD' function, against the contents of a
-- @.dat@ file. Dumps in another @.dat@ file. An example input file is
-- provided in the @files/test-equalizer@ folder in the root path of
-- the project.
testFilterScript inFile = do
  contents <- readFile inFile
  writeFile audioOut (writeS (audioFilterD (readS contents)))
  putStrLn $ "\nDone. Data dumped in " ++ audioOut
  where
    audioOut = takeDirectory inFile ++ "/AudioOutFSD.ext"

-- | Tests the
-- 'ForSyDe.Shallow.Example.Synchronous.Equalizer.AudioFilter.audioAnalyzer'
-- function, against the contents of a @.dat@ file. Dumps in another
-- @.dat@ file. An example input file is provided in the
-- @files/test-equalizer@ folder in the root path of the project.
testAnalyzerScript inFile = do
  fftInfile  <- openFile inFile ReadMode
  fftOutfile <- openFile outFile WriteMode
  contents   <- hGetContents fftInfile
--    hPutStr fftOutfile (writeS (audioAnalyzer 6 ((readS contents2) :: Signal Double)))
  putStr (show (audioAnalyzer pts ((readS contents) :: Signal Double)))
  hPutStr fftOutfile (writeS (audioAnalyzer pts ((readS contents) :: Signal Double)))
--    hClose fftOutfile
  putStr $ "\nDone. Data dumped in " ++ outFile ++ "\n"
  where
    k = 8
    n = 2 ^ k 
    sig = takeS (2*(2^k)) input
    input = 0.1 :- 0.2 :- input
    outFile = takeDirectory inFile ++ "/fftOut.dat"

-- | Chains and tests the
-- 'ForSyDe.Shallow.Example.Synchronous.Equalizer.AudioFilter.audioFilter' and
-- 'ForSyDe.Shallow.Example.Synchronous.Equalizer.AudioFilter.audioAnalyzer'
-- functions, against the contents of a @.dat@ file. Dumps in another
-- @.dat@ file. An example input file is provided in the
-- @files/test-equalizer@ folder in the root path of the project.
testFilterAndAnalyzerScript inFile = do 
  -- Test AudioFilter
  putStr "\n-->Test AudioFilter \n"
  filterInfile     <- openFile inFile ReadMode
  filterContents   <- hGetContents filterInfile 
  putStr (show (audioFilterD (readS filterContents)))
  writeFile audioOut (writeS (audioFilterD (readS filterContents)))
  putStrLn $ "\nDone. Data dumped in " ++ audioOut  
-- Test AudioAnalyzer
  putStr "\n--> Test AudioAnalyzer \n"
  analyzerInfile   <- openFile audioOut ReadMode
  analyzerOutfile  <- openFile analyzerOut WriteMode
  analyzerContents <- hGetContents analyzerInfile
  putStr (show (audioAnalyzer pts ((readS analyzerContents) :: Signal Double)))
  hPutStr analyzerOutfile (writeS (audioAnalyzer pts ((readS analyzerContents) :: Signal Double)))         
  putStrLn $ "\nDone. Data dumped in " ++ analyzerOut
  --fftInfile <- openFile "audioOut.txt" ReadMode
  --fftOutfile <- openFile "fftOut.txt" WriteMode
  --contents <- hGetContents fftInfile
  --putStr (writeS (((readS contents) :: Signal Double)))
  --hPutStr fftOutfile (writeS (((readS contents) :: Signal Double)))
  where
    audioOut    = takeDirectory inFile ++ "/audioOut.dat"
    analyzerOut = takeDirectory inFile ++ "/analyzerOut.dat"

