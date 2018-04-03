{-# LANGUAGE CPP #-}
module Main where

import SFML.Window
import SFML.Graphics.RenderWindow
import SFML.Graphics.Color
import SFML.System.Vector2 (Vec2f (..))

import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad (liftM, when)
import Control.Monad.IO.Class (liftIO)

import System.Random (StdGen)
import Component.Draw.Animation.SpriteSheet (SpriteSheet (..), loadSpriteSheet)
import Component.Draw.CircleDrawing (createCenteredCircle)
import Component.Input.Input (mousePosition)
import Component.Behavior.MousePointerBehavior (mousePositionCopier)
import qualified Component.Input.Input as I
import Physics.PhysicsWorld (createWorld, initPhysicsLibrary)
import Physics.PhysicsTypes (PhysicsWorld)
import Physics.PhysicsMessage
import Physics.PhysicsLayer (setLayers)

import Vec2.Vec2Math (zero)
import GameEnv (GameEnvironment (..), createGameEnv)
import GameObject.GameObject (GameObject)
import GameObject.GameObjectTypes
import GameObjectFactory (createStaticGameObjectB)
import PrefabObjects.TriangleMouseFollower (createMouseFollowerEqTriangle)
import PrefabObjects.AnimatedBlueBird (createAnimatedBlueBird)
import PrefabObjects.BallInputAware (createBallInputAware)
import PrefabObjects.AnimatedRunningCat (createAnimatedRunningCat)
import PrefabObjects.AnimatedSpinningCoin (createSpinningCoin)
import PrefabObjects.MousePositionPrinter (mkMousePrinter)
import PrefabObjects.MouseClickListener (mkMouseClickListener)
import PrefabObjects.CollisionPointsCounter (mkCollisionPointsCounter)
import PrefabObjects.MouseInputPhysicsBall (mkMouseInputPhysicsBall)
import ObjectsFactory
import Component.Position
import qualified Component.Position as Pos
import System.GameSystem (startGame)
import System.GameWorld (GameWorld (..), GameScene (..))
import System.Messaging.PhysicsInbox
import Random.Random
import Data.Time

defaultGravity :: Float
defaultGravity = 30

shipInitialPosition :: Vec2f
shipInitialPosition = Vec2f 0 400

main :: IO ()
main = do
    initPhysicsLibrary
    physicsWorld <- createWorld defaultGravity

    let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
    wnd <- createRenderWindow (VideoMode 640 480 32) "Falling Balls Demo" [SFDefaultStyle] ctxSettings
    
    -- Game Environment initialization
    dimensions <- getWindowSize wnd
    currTime <- getCurrentTime
    let gameEnv = createGameEnv dimensions currTime

    -- Initialize a Random Generator
#ifdef USE_RANDOM_GENERATOR
    gen <- newGenerator
#else
    gen <- newGeneratorFromString "1458194910 1"
#endif

    objects  <- runMaybeT (createObjects gen gameEnv physicsWorld)
    case objects of 
        Nothing -> putStrLn "Error creating game objects"
        Just balls -> do
                        let world = GameWorld wnd
                            scene = GameScene physicsWorld balls ()
                        startGame world scene gameEnv
                        putStrLn "This is the End!"


type BallCreation a = ReaderT GameEnvironment (StateT StdGen (MaybeT IO)) a

runBallCreation :: StdGen -> GameEnvironment -> BallCreation a -> MaybeT IO (a, StdGen)
runBallCreation gen env eval = runStateT (runReaderT eval env) gen

createObjects :: StdGen -> GameEnvironment -> PhysicsWorld -> MaybeT IO [GameObject st]
createObjects gen env space = do 
    mouseListener <- liftM (`Pos.setPosition` Vec2f 450 200) mkMouseClickListener
    mousePrinter <- liftM (`Pos.setPosition` Vec2f 500 0) mkMousePrinter
    goCounter <- createLiveGameObjectCounter (Vec2f 20 20)
    ballOnMouse <- ballAtMousePosition
    return [ballOnMouse, mouseListener, mousePrinter, goCounter]

mouseXPosition :: BehaviorType st
mouseXPosition obj = do
    (Vec2f mouseX _) <- mousePosition
    let (Vec2f _ objY) = getPosition obj
        newPosition = Vec2f mouseX objY
    return (setPosition obj newPosition)

ballAtMousePosition :: MaybeT IO (GameObject st)
ballAtMousePosition = do
    drw <- createCenteredCircle 10.0 green
    let beh = Behavior mouseXPosition
        pos = shipInitialPosition
    return (createStaticGameObjectB drw pos beh)