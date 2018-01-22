# AHaskellGame

This is my final project for the course on Game Development I'm currently enrolled. The aim is to explore the capabilities of Game Programming on a purely functional programming language (Haskell) while at the sime time, hopefully, getting the grasp of this amazing language. 

I'm still unsure whether it will be a simple game or a more ambitious yet with a more reduced scope of a simple game engine. 

## Diary

### 13/01/2018 - Some abstractions are starting to show up

![alt text][diary-01]

Yes, not a whole lot of action, but I'm quite satisfied with my progress so far. I was able to write a somewhat agnostic DrawingComponent and BehaviorComponent. They are quite limited so far, but what started to be a simple hardcoded gameObject (Ball.hs, see the earliest commits) is starting to become a really simple container for abstract components.

One of the things that I will work on the following week will probably be:
  - Input System
  - Sprites
  - GameObjects and Behavior that act upon events (Input, most probably)

### 21/01/2018 - Higher order behavior functions

![alt text][diary-02]

  - The Ball module was finally replaced by the GameObject data type (refactoring).
  - Created some objects to make use of the mouse position passed as an environment in the Reader Monad.
  - Composition of different behaviors using the (>>=) operator, one of the nicest findings, IMO. Eg: Follows the current mouse position as well as pointing towards it at all times. (White triangle with two yellow balls).
 
```
followPointingMouseB :: Behavior
followPointingMouseB = Behavior $ (mousePointer =<< ) . mouseFollower
```
  - Another thing I'm actually proud of is the possibility of changing the current behavior based on runtime decisions.
		This is done unsing the setBehaviorT method, example:

```
deathByUpdates :: Int -> BehaviorType
deathByUpdates x = behaviorPred (x < 0) dieBehavior (deathByUpdates (x - 1))

behaviorPred :: Bool -> BehaviorType -> BehaviorType -> BehaviorType
behaviorPred bool fst snd obj = let
	chosenBehavior = if bool then fst else snd
	in
		return $ setBehaviorT chosenBehavior obj
```

The behaviorPred is also the first Higher order function I was able to create using my own datatypes. It will be moved from the current module it resides, don't worry.

Other honorable mentions are:
  - Composite Drawing, a single game object with multiple drawings (represented by the triangle with the two yellow balls).
  - Random creation of game objects using the StateT monad together with StdGen.
      Also used a "LANGUAGE CPP" ghc language extension to enable the use of conditional directives just out of curiosity.
  - Add rotation to game objects (The white hexagon).
  - Add suport for text drawings
  - Stateful behaviors which complements what was said above.
  - Added the first sprite to the game. (It was about time, right)?
  - The last meaningful change was the added ability that gameobjects have to create child objects (The three messy "This is ..." were created that way). I still haven't created a behavior to actually trigger the creation of a child, though.

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

```
Creating circle R: 5.0C: Color {r = 255, g = 255, b = 255, a = 255}
Creating triangle at Vec2f 150.0 150.0
AHaskellGame-exe: divide by zero
```

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

```
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
```

This is also in honor for being the first logic bug on the project, and rightfully so because I haven't done any testing whatsoever prior to this event (my bad!).


[diary-01]: https://github.com/lhcopetti/AHaskellGame/raw/develop/DOCs/Diary/2018-01-13_AHaskellGame.gif "Diary 13/01/2018"
[diary-02]: https://github.com/lhcopetti/AHaskellGame/raw/develop/DOCs/Diary/2018-01-21_AHaskellGame.gif "Diary 21/01/2018"
