{-# LANGUAGE CPP #-}
module Main where

import SFML.Window
import SFML.Graphics.RenderWindow
import SFML.Graphics.Color
import SFML.System.Vector2 (Vec2f (..))

import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad (liftM)

import System.Random (StdGen)
import System.Messaging.Messages.TextDrawingMessage (setTextMsg)
import System.Messaging.Handler.PushMessageHandler (pushMessage)
import Component.Draw.TextDrawing (createText)
import Component.Draw.ConvexDrawing (createConvex)
import Component.Input.Input (mousePosition)
import Component.Behavior.Behaviors (deadManWalkingB, behaveBothB, behaveOutOfBoundsB, deathByUpdatesB)
import Component.Behavior.HigherOrderBehavior (behaveAll)
import Component.Behavior.InputBehavior (behaveOnMousePress, behaveOnMouseJustPressed)
import Physics.PhysicsWorld (createWorld, initPhysicsLibrary)
import Physics.PhysicsTypes (PhysicsWorld)
import Physics.PhysicsMessage
import Physics.CirclePhysics (mkCirclePhysicsD)

import Vec2.Vec2Math (zero)
import GameEnv (GameEnvironment (..), createGameEnv)
import GameObject.GameObjectTypes
import GameObjectFactory (createStaticGameObjectB, createGameObject)
import PrefabObjects.MousePositionPrinter (mkMousePrinter)
import PrefabObjects.MouseClickListener (mkMouseClickListener)
import ObjectsFactory
import ChildBearer
import qualified Component.Position as Pos
import System.GameSystem (startGame)
import System.GameWorld (GameWorld (..), GameScene (..))
import System.GameScene (newSceneState)
import System.Messaging.PhysicsInbox
import System.Input.MouseSnapshot (MButton (..))
import Random.Random
import Data.Time
import Updatable

data GameState = GS { score :: Integer
                    }

defaultGravity :: Float
defaultGravity = 30

shipInitialPosition :: Vec2f
shipInitialPosition = Vec2f 0 400

shipSize :: Float
shipSize = 20

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
                            sceneState = newSceneState (GS 3)
                            scene = GameScene physicsWorld balls sceneState
                        startGame world scene gameEnv
                        putStrLn "This is the End!"


type BallCreation a = ReaderT GameEnvironment (StateT StdGen (MaybeT IO)) a

runBallCreation :: StdGen -> GameEnvironment -> BallCreation a -> MaybeT IO (a, StdGen)
runBallCreation gen env eval = runStateT (runReaderT eval env) gen

createObjects :: StdGen -> GameEnvironment -> PhysicsWorld -> MaybeT IO [GameObject GameState]
createObjects _ _ space = do 
    mouseListener <- liftM (`Pos.setPosition` Vec2f 450 200) mkMouseClickListener
    mousePrinter <- liftM (`Pos.setPosition` Vec2f 500 0) mkMousePrinter
    goCounter <- createLiveGameObjectCounter (Vec2f 20 20)
    ballOnMouse <- ballAtMousePosition
    enemy <- mkEnemy space
    scoreCounter <- mkScoreCounter (Vec2f 100 100) 20
    bounds <- createBounds space
    return $ [scoreCounter, enemy, ballOnMouse, mouseListener, mousePrinter, goCounter] ++ bounds


mkScoreCounter :: Vec2f -> Int -> GameObjectCreation GameState
mkScoreCounter pos fontSize = do
    drw <- createText fontSize "Score counter: 0"
    let beh = Behavior updateScoreBehavior
    return (createStaticGameObjectB drw pos beh)

updateScoreBehavior :: BehaviorType GameState
updateScoreBehavior obj = do
    currentScore <- gets (score . userState)
    let msg = setTextMsg ("Score counter: " ++ show currentScore)
    pushMessage msg obj

mkEnemy :: PhysicsWorld -> GameObjectCreation GameState
mkEnemy world = do
    (phy, drw) <- mkCirclePhysicsD 10.0 (Vec2f 150 0) world
    let beh = enemyDeathBehavior
        pos = Vec2f 0 0
    return (createGameObject drw beh phy pos)

enemyDeathBehavior :: Behavior GameState
enemyDeathBehavior = let 
    fst' = enemyEscapedBehavior 
    snd' = deadManWalkingB
    in 
        behaveOutOfBoundsB (behaveBothB fst' snd')

enemyEscapedBehavior :: Behavior GameState
enemyEscapedBehavior = Behavior $ \obj -> do
    modify decrementScore
    return obj

decrementScore :: SceneState GameState -> SceneState GameState
decrementScore state = state { userState = GS newScore }
    where
        newScore = (score . userState $ state) - 1

mouseXBehavior :: BehaviorType st
mouseXBehavior obj = do
    (Vec2f mouseX _) <- mousePosition
    let (Vec2f _ objY) = Pos.getPosition obj
        newPosition = Vec2f mouseX objY
    return (Pos.setPosition obj newPosition)

ballAtMousePosition :: MaybeT IO (GameObject st)
ballAtMousePosition = do
    drw <- createConvex green (mkPlayerTriangle shipSize)
    let beh = Behavior (behaveAll [mouseXBehavior, shootMissile, shootMissile'])
        pos = shipInitialPosition
    return (createStaticGameObjectB drw pos beh)

shootMissile :: BehaviorType st
shootMissile = behaveOnMouseJustPressed MLeft onMousePress
    where
        onMousePress obj = do
            mousePos <- mouseYLockedPosition
            return $ addChildP (mkMissile mousePos) obj

shootMissile' :: BehaviorType st
shootMissile' = behaveOnMousePress MRight onMousePress
    where
        onMousePress obj = do
            mousePos <- mouseYLockedPosition
            return $ addChildP (mkMissile mousePos) obj

mouseYLockedPosition :: UpdateMStack Vec2f st
mouseYLockedPosition = do
    (Vec2f x' _) <- mousePosition
    let (Vec2f _ y') = shipInitialPosition
    return $ Vec2f x' (y' - shipSize)

mkMissile :: Vec2f -> PhysicsWorld -> GameObjectCreation st
mkMissile pos phyWorld = do
    (phy, drw) <- mkCirclePhysicsD 5.0 pos phyWorld
    let msgs = PMSG <$> [applyOnlyForce (Vec2f 0 (-5000)) zero]
        beh = deathByUpdatesB 500
        obj = createGameObject drw beh phy pos
    return (foldr addInbox obj msgs)


mkPlayerTriangle :: Float -> [Vec2f]
mkPlayerTriangle size = map (\f -> f size) [ \x -> Vec2f 0 (-x / 2)
                                    , \x -> Vec2f x (x / 2)
                                    , \x -> Vec2f (-x) (x / 2) ]

createBounds :: PhysicsWorld -> MaybeT IO [GameObject st]
createBounds world = do
    left <- createPhysicsLine 10.0 (Vec2f 0 0, Vec2f 0 640) world
    right <- createPhysicsLine 10.0 (Vec2f 640 0, Vec2f 640 480) world
    up <- createPhysicsLine 10.0 (Vec2f 0 0, Vec2f 640 0) world
    return [left, right, up]
