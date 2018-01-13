# AHaskellGame

This is my final project for the course on Game Development I'm currently enrolled. The aim is to explore the capabilities of Game Programming on a purely functional programming language (Haskell) while at the sime time, hopefully, getting the grasp of this amazing language. 

I'm still unsure whether it will be a simple game or a more ambitious yet with a more reduced scope of a simple game engine. 

## Diary

* 13/01/2017 - Some abstractions are starting to show up

![alt text][diary-01]

Yes, not a whole lot of action, but I'm quite satisfied with my progress so far. I was able to write a somewhat agnostic DrawingComponent and BehaviorComponent. They are quite limited so far, but what started to be a simple hardcoded gameObject (Ball.hs, see the earliest commits) is starting to become a really simple container for abstract components.

One of the things that I will work on the following week will probably be:
  - Input System
  - Sprites
  - GameObjects and Behavior that act upon events (Input, most probably)

  
## Development

### Setting up dependencies

The first thing you should do is install the SFML libraries in your system. The compilation below will not work unless it can find all the SFML dependencies it needs (The SFML library and the CSFML bindings for the C language).

#### Windows

I have not bothered trying installation on Windows as I have been running Linux as my primary system as of late, but you should have no big difficulties because stack manages most of the dependencies and SFML has precompiled binaries for Windows. Should you find anything out of the ordinary, please, send me the instructions or open a MR and I will hapilly apply it here.

#### Linux

* Tested on Ubuntu only

### Building

To compile the executable, use: 

`
stack build
`

### Testing

To run the unit tests:

`
stack test
`

### Executing

`
stack exec AHaskellGame-exe
`

### Debugging (Trace)

Sometimes it may be helpful to get some insight as to why something is going bad. Take this trace for example:

`
Creating circle R: 5.0C: Color {r = 255, g = 255, b = 255, a = 255}
Creating triangle at Vec2f 150.0 150.0
AHaskellGame-exe: divide by zero
lhcopetti@lhcopetti-Vostro-5470:~/Documents/Dev/Haskell/SFMLHelloWorld$
`

The execution simply halts on that not so glorifying math error. After some research, I found out that you need to enable some flags on compilation and execution to actually get some information about that exception. 

To build, change the command line to:

`
stack build --profile
`
This will recompile the whole project with some important debugging information active.

To run and actually put those flags to good use, execute the binary like this:

`
stack exec AHaskellGame-exe +RTC --profile --trace --rts-options -xc
`

And simply wait for the error to ocurr again, however, this time, you will be presented with a lot more than a simple obscure error message:

`
*** Exception (reporting due to +RTS -xc): (THUNK_STATIC), stack trace:
  GHC.Real.CAF
  --> evaluated by: Behavior.BoxedBehavior.wrapAround,
  called from Component.Behavior.EnclosedBehavior.encloseByWrapAround.newObject,
  called from Control.Monad.Trans.Reader.>>=.\,
  called from Control.Monad.Trans.Reader.>>=,
  called from Component.Behavior.EnclosedBehavior.encloseByWrapAround,
  called from Component.Behavior.Behavior.behave,
  called from GameObject.Ball.update,
  called from GameObject.AnyGameObject.updateAnyGameObject,
  called from Control.Monad.Trans.Reader.fmap,
  called from Control.Monad.Trans.Reader.<*>.\,
  called from Control.Monad.Trans.Reader.<*>,
  called from Main.gameLoop.newObjs,
  called from Main.gameLoop,
  called from Main.loop,
  called from Main.main
AHaskellGame-exe: divide by zero
`

This is also in honor for being the first logic bug on the project, and rightfully so because I haven't done any testing whatsoever prior to this event (my bad!).


[diary-01]: https://github.com/lhcopetti/AHaskellGame/raw/develop/DOCs/Diary/2018-01-13_AHaskellGame.gif "Diary 13/01/2018"