forsyde-haskell-demonstrators
===============================

Demonstrators implemented with the ForSyDe-Haskell Shallow modeling library. 

----

Installation and usage
----------------------
Most of the demos are intedended to be executed directly in `ghci`. 

Use `cabal` to compile the documentation:

	cabal configure
	cabal haddock
	

List of demos
-------------

#### 1. Synchronous MoC

  1. TrafficLight: Models a FSM for a traffic light controller.

  2. PitchControl: 
  TODO
  
  3. FibonacciRabbits: Models to Fibonacci rabbits reproduction system.
  
#### 2. Untimed MoC

  1. AdaptiveAmp: Models an adaptive amplifier.

  2. Ascii2Bin: Models two processes to convert between ASCII
     characters and their binary representations.


#### 3. Synchronous DataFlow MoC
  
  No demos available at the moment.

#### 4. Continuous Time MoC

  No demos available at the moment.


#### 5. Heterogeneous Models
  
  These are demos that show how to use more than one MoC to model a system.

  1. ASKTransceiver: 
  TODO
