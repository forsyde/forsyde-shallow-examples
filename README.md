forsyde-haskell-demonstrators
===============================

Demonstrators implemented with the ForSyDe-Haskell Shallow modeling library. 

----

Installation
------------

The example applications are provided as extensions to the `ForSyDe.Shallow` library, and can be imported or run in a `ghci` session. 

To install the applications globaly, you can use the following commands, provided you have installed Cabal:

    cabal install    # installs 'forsyde-shallow-examples' and all its dependencies globally
    ghci             # opens a GHC interpreter session from which you can import the applications
    
To install the applications locally in a sandbox, you can use the following commands:
    
    cabal sandbox init # creates a sandbox in the current folder
    
    # in case 'ForSyDe.Shallow' is not globally installed, you need to download/clone its source repo, and type
    cabal sandbox add-source <path/to/forsyde-shallow>
    
    cabal install      # installs 'forsyde-shallow-examples' and all its dependencies locally, in the sandbox
    cabal repl         # opens a GHC interpreter session for the sandbox from which you can import the applications

Each application is documented in-line using Haddock style comments. You are strongly advised to compile and consult the documentation before running the applications. You need to install [Haddock](https://www.haskell.org/haddock/) prior to compiling the documentation. 

    cabal install haddock             # installs the latest version of Haddock from HackageDB
    cabal configure                   # prepares the sources for documentation
    cabal haddock --hyperlink-source  # generates a HTML documentation
    
The path to the `index.html` file which you need to open with a browser is printed out on the terminal if the command succeeds.

Usage
-----

Once you have installed the applications (globally or sandboxed), open an GHC interpreter session (see above for the right command). Each application can be imported as a module under `ForSyDe.Shallow.Example`. For example, to load and test the `RCFilter` application here is how the session could look like:

    Prelude> import ForSyDe.Shallow.Example.Synchronous.RCFilter as RCFilter
    Prelude RCFilter> simulate 1 1 1 10
    {0.0,0.5,0.75,0.875,0.9375,0.96875,0.984375,0.9921875,0.99609375,0.998046875}
    Prelude RCFilter> plotOutput 1e3 1e-6 1e-6 10000
    "Signal(s) output plotted."

**NOTE:** most plotting functions provided depend on [gnuplot](http://www.gnuplot.info/), so make sure you have it installed and accessible in case you want to use plots.

List of demos
-------------

#### 1. Synchronous MoC

  1. `TrafficLight`: Models a FSM for a traffic light controller.
  1. `PitchControl`: Models a Proportional Integral Derivative (PID) control case study for the pitch of a Boeing's commercial aircraf.
  1. `FibonacciRabbits`: Models to Fibonacci rabbits reproduction system.
  1. `FibonacciRabbitsDeath`: Models the Fibonacci reproduction system considering rabbits death rate.
  1. `RCFilter`: Models a first order low pass RC filter.

#### 2. Untimed MoC

  1. `AdaptiveAmp`: Models an adaptive amplifier.
  1. `Ascii2Bin`: Models two processes to convert between ASCII characters and their binary representations.

#### 3. Synchronous DataFlow MoC
  
  No demos available at the moment.

#### 4. Continuous Time MoC

  No demos available at the moment.


#### 5. Heterogeneous Models
  
  These are demos that show how to use more than one MoC to model a system.

  1. `ASKTransceiver`: Models an Asynchronous Shift Key Trasciever, using the SY, SDF and CT models of computation.
