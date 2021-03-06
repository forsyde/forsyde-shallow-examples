forsyde-shallow-examples
========================

Demonstrators implemented with the ForSyDe-Haskell Shallow modeling library. 

----

Installation
------------

The example applications are provided as extensions to the [`ForSyDe.Shallow`](https://github.com/forsyde/forsyde-shallow) library, and can be imported or run in a `ghci` session. Please make sure you acquire `ForSyDe-Shallow` prior to installing the examples.

To install the applications globaly, you need to have installed `ForSyDe-Shallow` globally. Then, using the Cabal package manager, you can type in:

    cabal update     # updates the cabal repositories information
    cabal install    # installs 'forsyde-shallow-examples' and all its dependencies globally
    ghci             # opens a GHC interpreter session from which you can import the applications
    
To install the applications locally in a sandbox, you still need to acquire the `ForSyDe-Shallow` source project, but it does not need to be installed. You can use the following commands:
    
    cabal update       # updates the latest repositories information
    cabal sandbox init # creates a sandbox in the current folder
    cabal install      # installs 'forsyde-shallow-examples' and all its dependencies locally, in the sandbox
    cabal repl         # opens a GHC interpreter session for the sandbox from which you can import the applications

Each application is documented in-line using Haddock style comments. You are strongly advised to compile and consult the documentation before running the applications. You need to install [Haddock](https://www.haskell.org/haddock/) prior to compiling the documentation. 

    cabal install haddock hscolour    # installs tools for generating documentation
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

  1. [`TrafficLight`](src/ForSyDe/Shallow/Example/Synchronous/TrafficLight.hs): Models a FSM for a traffic light controller.
  1. [`PitchControl`](src/ForSyDe/Shallow/Example/Synchronous/PitchControl.hs): Models a Proportional Integral Derivative (PID) control case study for the pitch of a Boeing's commercial aircraf.
  1. [`FibonacciRabbits`](src/ForSyDe/Shallow/Example/Synchronous/FibonacciRabbits.hs): Models to Fibonacci rabbits reproduction system.
  1. [`FibonacciRabbitsDeath`](src/ForSyDe/Shallow/Example/Synchronous/FibonacciRabbitsDeath.hs): Models the Fibonacci reproduction system considering rabbits death rate.
  1. [`RCFilter`](src/ForSyDe/Shallow/Example/Synchronous/RCFilter.hs): Models a first order low pass RC filter.
  1. [`MulAcc`](src/ForSyDe/Shallow/Example/Synchronous/MulAcc.hs) : Models a multiply-accumulator. Adaptation from a [ForSyDe-SystemC](https://github.com/forsyde/ForSyDe-SystemC) version, bridging between the two languages.
  1. [`Equalizer`](src/ForSyDe/Shallow/Example/Synchronous/Equalizer.hs): Models an equalizer system used in [Sander's PhD thesis](http://urn.kb.se/resolve?urn=urn%3Anbn%3Ase%3Akth%3Adiva-3525). 

#### 2. Untimed MoC

  1. [`AdaptiveAmp`](src/ForSyDe/Shallow/Example/Untimed/AdaptiveAmp.hs): Models an adaptive amplifier.
  1. [`Ascii2Bin`](src/ForSyDe/Shallow/Example/Untimed/Ascii2Bin.hs): Models two processes to convert between ASCII characters and their binary representations.

#### 3. Synchronous DataFlow MoC
  
  1. [`ImageProcessing`](src/ForSyDe/Shallow/Example/SDF/ImageProcessing.hs): Models a toy image processing example used as system specification for the [KTH IL2212 Embedded Software](https://www.kth.se/student/kurser/kurs/IL2212?l=en) lab project. It introduces the concept of parallel patterns as skeletons on vectors.

#### 3. Senario Aware DataFlow MoC
  
  1. [`RISC`](src/ForSyDe/Shallow/Example/SADF/RISC.hs): Models a RISC processor featuring an extensible instruction set, using the concept of scenarios. This model has been used in the paper of [Bonna et al. (2019)]().
  1. [`MPEG4`](src/ForSyDe/Shallow/Example/SADF/MPEG4.hs): Models a prototype MPEG4 Simple Profile Decoder. This model has been used in the paper of [Bonna et al. (2019)]().
  
#### 4. Continuous Time MoC

  No demos available at the moment.


#### 5. Heterogeneous Models
  
  These are demos that show how to use more than one MoC to model a system.

  1.  [`ASKTransceiver`](src/ForSyDe/Shallow/Example/Heterogeneous/ASKTransceiver.hs): Models an Asynchronous Shift Key Trasciever, using the SY, SDF and CT models of computation.
