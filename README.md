[![Build Status](https://travis-ci.org/lhcopetti/AHaskellGame.svg?branch=develop)](https://travis-ci.org/lhcopetti/AHaskellGame) 

# AHaskellGame

This is my final project for the course on Game Development I'm currently enrolled. The aim is to explore the capabilities of Game Programming on a purely functional programming language (Haskell) while at the sime time, hopefully, getting the grasp of this amazing language. 

I'm still unsure whether it will be a simple game or a more ambitious yet with a more reduced scope of a simple game engine. 

## News

Freshly baked and updated Docker support. Read all about it [here](DOCs/Docker.md)!

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

### 28/01/2018 - Animations and Named Messages

![alt text][diary-03]

It is time for another recap! Yay!

I will try to keep these more minimalistic, I got carried away in the last one and endeup writing a wall of text by enumerating everything I had done. 

- Primitive input system

  This was just an experiment see how I would pack input handling in the current 'architecture'. The ball that 'dies' after pressing the 'q' key is using this feature.

- Named messages for composite drawings

  All drawings have the ability to receive messages `Drawing -> IO ()`, the difference is that for composite drawings the messages would be sent to all of the underlying drawings, sometimes this is desired behavior but more often than not, counter-intuitive. The pair 'title' and 'subtitle' uses these kind of messages to update their texts individually even though they are part of the same GameObject entity.

- Animation

  Handling for animated drawings and automatic spritesheet cutting. There is still a lot of ground to cover but still, it looks good. One of the last refactorings I did this week was moving this animation behavior to fit inside a DrawingComponent which was really nice (It wasn't born this way).

### 04/02/2018 - A sabbatical week

I decided to take a week-long break from coding into the repository. I'm using the free time instead to expand my current knowledge on some of the not so trivial haskell topics.

- StateVar

A simple [package](https://hackage.haskell.org/package/StateVar-1.1.0.4/docs/Data-StateVar.html) that is used to encapsulate getters and setters references in the IO monad. I came across StateVar while playing around with the [Hipmunk Playground](https://hackage.haskell.org/package/HipmunkPlayground). [Hipmunk](https://hackage.haskell.org/package/Hipmunk) is a physics library based upon [Chipmunk](https://chipmunk-physics.net/). As I'm inclined to integrate it into the codebase learning more about it certainly won't hurt.

- Lens library

I also used some of the new spare time to delve into the [Lens library](https://hackage.haskell.org/package/lens), which is a lot simpler to use than to understand how it works. I decided to keep my first toy program in a [Gist](https://gist.github.com/lhcopetti/ec27f86c54cc31b3c5e9bf98817ec962) (implementing a monadic version of the Vigenere Cipher, an exercise inspired by one of the exercises on the "Programming in Haskell" book) for future reference.

- Arrows

The Arrows have come to my attention after reading [this introduction](http://www.haskellforall.com/2014/03/introductions-to-advanced-haskell-topics.html) into some advanced haskell topics where it suggests reading this [paper](http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf). Even though I am still far from the end I really liked the abstraction and I may rewrite some routines to make use of it.

- Book: Haskell Programming

I recently purchased [Haskell Programming](http://haskellbook.com/) and some of the time was also invested into going further into the book. Currently at Chapter 13, I have learned some interesting things, even though the content is still quite beginnerish.

### 11/02/2018 - Did anyone say physics?

![alt text][diary-04]

I am proud to announce that AHaskellGame is now capable of handling physics using the [Hipmunk library](https://hackage.haskell.org/package/Hipmunk). It wasn't a particular productive week because the first four days were spent experimenting with Hipmunk by using the [Hipmunk Playground](https://hackage.haskell.org/package/HipmunkPlayground). Most of the work this week was done on the 3 days following that. The end result was pretty satisfying to watch considering in the beginning of the week I was quite terrified that I would to try to add another library, having to manage another set of pointers (C pointers) and unscapable IOs.

Enough rant, let's go back to topic mode:

- Physics

I finally added a dedicated physics library as mentioned above. Of couse, it is embrionary still but I am excited nonetheless. An interesting thing that happened is that I was able to keep my old 'physics' version intact. I didn't even consider that at the beginning, but now it is possible to have a simple game object that will not be part of the physics world but will still obey to the position += velocity updates.

```
data Physics = SimplePhy Vec2f Float
             | HipPhy H.Body H.Shape H.ShapeType
```

- Hipmunk

One thing that I'm really striving for is the complete isolation of the Hipmunk library. I'm keeping it so that should I ever change the physics library, all I would have to do is reimplement the core interface and rename the imports to point to the specific library. Example:

```
import qualified Physics.Library.Hipmunk.HipmunkCircle as HMP

mkCirclePhysics :: Float -> Vec2f -> PhysicsWorld -> IO Physics
--                           The call to the library specific function using the qualified import
mkCirclePhysics radius pos = HMP.mkCirclePhysics hRadius hPos
    where
        hRadius = realToFrac radius
        hPos    = vec2fToHVector pos
```
I could even switch back and forth using CPP defines to decide which library should be used. 

- Meta-Topic: The experience so far

This will probably become one of the most interesting programming sprints I have done so far. I feel like I am learning a lot of things and I'm starting to feel like functionally thinking is becoming easier as time goes by. That becomes particular apparent when I have to reach out to code I wrote only a month ago. Also, this time next week will be past half the due date for this project, so we'd better be going fast enough!

### 18/02/2018 - Did anyone say procastination?

![alt text][diary-05]

I feel like I really lost traction this week, as this diary entry is being written at least 5 days later than scheduled. I imagine that partly, this is because of the pressure that I recently started feeling about having something at least close to deliverable by the end of March. This has been a great experience so far but when it comes to actually settling down to prospect a result, I freeze. The argument that I employ is that for academic projects, I don't really care if what I wrote doesn't even have an entry point to be executed. All I care is everything that I learned, practiced and evolved as part of the experience through the project and programming in general. I usually state that the process is a lot more important to me than the end result. 

My focus is shifting, though, toward a more objective oriented approach to expose the better part of the project so that others might enjoy, without necessaringly having to look through the code.

Anyway, let's go back a bit and explore what I did do this past week:

- Toy program: Components

I made a toy program modelling the initialization of a group of components. In the case one of these components fails, they are deinitialized in reverse order so as to avoid
any dependencies implication, should there exist any. It is available here: [Gist](https://gist.github.com/lhcopetti/f58c510ec61f1e8eb287f15bc30dbdd4)

- Haskell C bindings 

Another activity that I am enjoying is playing around with the [Foreign](https://hackage.haskell.org/package/base-4.10.1.0/docs/Foreign.html) packages that allow Haskell to interface with another programming language. One of the biggest advantages that I am having is being able to understand how the Physics (Hipmunk) and Graphics (SFML) library bindings have been written, including the use of the .hsc file type.

- The only remarkable thing I did this week was improving the DebugDraw for Hipmunk. I have to confess that it was heavily inspired by Box2D and its test bed.

- MonadPlus

I had an epifany while scraping haskell questions on stackoverflow. It was not so much of an epiffany because said discovery was actually written in one of the answers, nonetheless, I felt that recognizable click going off on my brain. MonadPlus is the typeclass that models the possibility of choices and failures. And I have a really neat use case to show as to why implement your methods preferably by the use of monad type classes. The example below is how I got to apply the MonadPlus to actually achieve simpler code just by the smart use of the type system:

```
module MonadPlusTest where

import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Monad.Trans.Maybe

printInfo :: MaybeT IO String
printInfo = do
    str <- liftIO getLine
    -- res <- MaybeT . return . bigLengthOnly $ str
    res <- bigLengthOnly str
    return ("Info: " ++ str ++ " | Size: " ++ show res)


bigLengthOnly :: MonadPlus m => String -> m Integer
bigLengthOnly xs = do
    guard (length xs > 10)
    return (fromIntegral . length $ xs)
-- bigLengthOnly :: String -> Maybe Integer
-- bigLengthOnly xs = do
--     guard (length xs > 10)
--     return (fromIntegral . length $ xs)
```

### 11/03/2018 - The end is near!

I have spent the last couple of weeks weighing the pros and cons of developing a game or going for something more generic (a library for developing games in Haskell). I finally decided to go for the latter!  That means I have a exatcly 3 weeks to work on the remaining project.

##### My plan:

I am starting to feel the pressure and some of my decisions are already taking into account the proximity of the end of the project. Below, you will find a draft of my plan for the following weeks (the deliverable should be ready on March 31):

- *Implement 3 SIMPLE (even larger emphasis on simple) games*

The goal is to show that the 'framework' I built is sufficiently agnostic to embrace different game genres.

1) Conway's Game of Life - Almost ready (still not handling mouse clicks).
2) To be filled...
3) To be filled...

- *Maintain my hideous playground (the original executable)*

    I really like how messy it has become. For me, it shows how much the project has grown and I always feel really good whenever I see the amounts of features that are concurrently running on that sandbox.

- *Learn more about Docker*

I won't promise anything here, but I would like to at least try to apply the concept to my game, especially because it has native dependencies such as libcsfml and libsfml that could be packaged together to ease the deploy and continous integration phase (Travis).

Now, take a glimpse at the second executable (Conway's Game of Life) that I developed the past weeks.

![alt text][diary-06]

### Docker Support (YAY!!!!) - 08/04/2018

I decided to invest some time on learning Docker and what amazed me the most was that GUI apps could also be put inside containers, amazing isn't it?

Along the added ability of deploying precompilied images to run the AHaskellGame app, I was able to create an environment for realiable testing with Travis CI (Finally, I can proudly put the Travis CI build status badge on this readme file). Two birds with one stone (or whale), right?!

#### Docker Environment

Thanks to DockerHub, having access to this amazing piece of software could not be made easier.

I should warn you though, if you attempt to run a desktop GUI application using docker, it will most likely fail. The reason behind that is because the container doesn't have access to any of the desktop servers running on the host (eg: X). To allow the X server on your host machine to listen for incoming connections from docker containers you should execute the command below (more information [here](http://wiki.ros.org/docker/Tutorials/GUI)):

```
xhost +local:root
```

`ATTENTION` Please, don't forget to disable it after you are done with the following command! Leaving the connection open might put your in danger!

```
 xhost -local:root
```

To finally run the application, execute the following:

```
docker run --rm -it --env="DISPLAY"  --volume="/tmp/.X11-unix:/tmp/.X11-unix:rw" lhcopetti/haskell-game
```

This will put you inside the container with shell access (I didn't provide any ENTRYPOINTs or CMD in the Dockerfile, but that's desired behavior). From there, you can execute:

```
stack exec AHaskellGame-exe
or
stack exec ConwaysGameOfLife
or
stack exec FallingBalls
```

to see the different demos available. Any problems or suggestions, please let me know!

#### For the future:

* DONE! I am aware that this is more of a development image as it carries all of the infrastructure (including the whole source code) to build the binaries.
* DONE! Set up another image for local development using volume mappings (An interesting trick I learned from [here](https://medium.com/travis-on-docker/why-and-how-to-use-docker-for-development-a156c1de3b24))
* DONE! Set up Travis so that the CI itself can push newly baked images into my DockerHub repository, promptly updating the hosted images.

## Development

### Setting up dependencies

The first thing you should do is install the SFML libraries in your system. The compilation below will not work unless it can find all the SFML dependencies it needs (The SFML library and the CSFML bindings for the C language).

#### Windows

I have not bothered trying installation on Windows as I have been running Linux as my primary system as of late, but you should have no big difficulties because stack manages most of the dependencies and SFML has precompiled binaries for Windows. Should you find anything out of the ordinary, please, send me the instructions or open a MR and I will hapilly apply it here.

#### Linux

* Tested on Ubuntu only

### Building

To compile the executable, use: 

After upgrading my stack resolver to lts-10.4, I have to explicitly mention the '--extra-include-dirs' and '--extra-lib-dirs'. Don't ask me why!

`
stack build --extra-include-dirs="/home/lhcopetti/Documents/Dev/Tools/CSFML-2.3/include" --extra-lib-dirs="/home/lhcopetti/Documents/Dev/Tools/CSFML-2.3/lib"
`

The directory '/home/lhcopetti/Documents/Dev/Tools/CSFML-2.3/' is where I cloned the CSFML git repository.

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
[diary-03]: https://github.com/lhcopetti/AHaskellGame/raw/develop/DOCs/Diary/2018-01-28_AHaskellGame.gif "Diary 28/01/2018"
[diary-04]: https://github.com/lhcopetti/AHaskellGame/raw/develop/DOCs/Diary/2018-02-11_AHaskellGame.gif "Diary 11/02/2018"
[diary-05]: https://github.com/lhcopetti/AHaskellGame/raw/develop/DOCs/Diary/2018-02-18_AHaskellGame.gif "Diary 18/02/2018"
[diary-06]: https://github.com/lhcopetti/AHaskellGame/raw/develop/DOCs/Diary/2018-03-11_ConwaysGameOfLife.gif "Diary 11/03/2018"