{-# LANGUAGE CPP #-}
module Main where

import SFML.Window
import SFML.Graphics.RenderWindow
import SFML.Graphics.Color
import SFML.Graphics.Rect (FloatRect (..), floatRectContains)

import Control.Monad (liftM, when)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.State

import Physics.PhysicsWorld (createWorld, initPhysicsLibrary)
import Physics.PhysicsTypes (PhysicsWorld)
import System.Messaging.Messages.ShapeMessage
import System.Messaging.Handler.PushMessageHandler
import GameEnv (GameEnvironment (..), createGameEnv)
import GameObject.GameObject (GameObject)
import GameObject.GameObjectTypes
import GameObject.TextGameObject (createTextGO, createTextGO)
import Component.Behavior.Behavior (setBehavior)
import Component.Behavior.Behaviors
import Component.Input.Input (isJustPressed, mousePosition)
import ObjectsFactory
import GridGameObjectFactory
import Component.Draw.ZOrderable
import qualified Component.Position as Pos
import System.GameSystem (startGame)
import System.GameWorld (GameWorld (..), GameScene (..))
import qualified System.Input.MouseSnapshot as M (MButton (..))
import Vec2.Vec2Math
import PrefabObjects.MousePositionPrinter (mkMousePrinter)
import PrefabObjects.RunningTime (mkRunningTime)
import Conway
import Data.Time (getCurrentTime)

defaultGravity :: Float
defaultGravity = 30

data SceneState = SceneState ConwayWorld Bool

main :: IO ()
main = do
#ifdef DO_SOMETHING_DIFFERENT
    putStrLn "Doing something different!"
#endif
    initPhysicsLibrary
    physicsWorld <- createWorld defaultGravity
    desktopMode <- getDesktopMode
    fsModes <- getFullscreenModes

    putStrLn $ "Current desktop Mode is: " ++ show desktopMode
    putStrLn ""
    putStrLn "Fullscreen modes: "
    putStrLn ""

    mapM_ (\m -> print m >> putStrLn "") fsModes

    let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
    wnd <- createRenderWindow (VideoMode 640 480 32) "Conway's Game of Life" [SFDefaultStyle] ctxSettings

    -- Game Environment initialization
    dimensions <- getWindowSize wnd
    currTime <- getCurrentTime
    let gameEnv = createGameEnv dimensions currTime

    objs <- runMaybeT (createObjects gameEnv physicsWorld)
    case objs of 
        Nothing -> putStrLn "Error creating game objects"
        Just (b, objects) -> do
                            let n = SceneState (initialBoard b) False
                                world = GameWorld wnd
                                scene = GameScene physicsWorld objects n
                            startGame world scene gameEnv 
                            putStrLn "This is the End!"

createObjects :: GameEnvironment -> PhysicsWorld -> MaybeT IO (ConwayWorld, [GameObject SceneState])
createObjects _ _ = do
    let gridSize = (15, 15)
    board <- newConwayWorld gridSize
    objs <- createGridObjects gridSize mkConwayCell
    stepper <- createLogicGO (stepConway 20)
    resetter <- createLogicGO (resetConwayB KeyR)
    shouldUpdate <- createLogicGO (turnOnAutoUpdateB KeyA)
    dontUpdate <- createLogicGO (turnOffAutoUpdateB KeyZ)
    manualStepper <- createLogicGO (singleStepB KeyS)
    instructions <- createInstructions
    runningTime <- liftM (`Pos.setPosition` Vec2f 400 470) mkRunningTime
    mouse <- liftM (`Pos.setPosition` Vec2f 0 470) mkMousePrinter
    let mouse' = setZ 10 mouse
    return (board, runningTime : mouse' : manualStepper : dontUpdate : shouldUpdate : stepper : resetter : objs ++ instructions)

initialBoard :: ConwayWorld -> ConwayWorld
initialBoard = setLives [ (2, 3), (3, 3), (4, 3), (6,3), (7, 3), (8, 3)
                        , (2, 4), (8, 4)
                        , (4, 5), (6, 5)
                        , (3, 6), (4, 6), (6, 6), (7, 6)
                        , (2, 7), (8, 7) ]

mkConwayCell :: Position -> MaybeT IO (GameObject SceneState)
mkConwayCell gpos@(x, y) = liftM (setBehavior behaviors) createObject
    where
        fx = fromIntegral x
        fy = fromIntegral y
        squareSize = 25
        sizeDrawings = 25 + 2 -- padding
        pos = addVec2f (Vec2f 2 2) (Vec2f (fx * sizeDrawings) (fy * sizeDrawings))
        cellRect = mkCellRect gpos squareSize (Vec2f 2 2) 2
        setColorB = Behavior (setConwayColorBehavior gpos)
        mouseToggleB = Behavior (toggleCellBehavior gpos cellRect)
        behaviors = behaveAllB [setColorB, mouseToggleB]
        createObject   = createSquareObject squareSize white pos

mkCellRect :: Position -> Float -> Vec2f -> Float -> FloatRect
mkCellRect (x, y) side (Vec2f ox oy) padding = let
    fx = fromIntegral x
    fy = fromIntegral y
    left = ox + fx * (side + padding)
    top =  oy + fy * (side + padding)
    in 
        FloatRect left top side side

toggleCellBehavior :: Position -> FloatRect -> BehaviorType SceneState
toggleCellBehavior pos rect obj = do
    press <- isJustPressed M.MLeft
    (Vec2f mx my) <- mousePosition
    let shouldToggle = press && floatRectContains mx my rect
    when shouldToggle (modify $ toggleBoard pos)
    return obj

stepConway :: Int -> Behavior SceneState
stepConway interval' = behaveEveryB interval' $ \go -> do
    (SceneState _ shouldUpdate) <- get
    behave (chooseBehaviorB shouldUpdate should shouldNot) go
        where
            should      = Behavior $ \go -> modify tickState >> return go
            shouldNot   = noopB

resetConwayB :: KeyCode -> Behavior SceneState
resetConwayB key = behaveOnKeyJustPressedB key $ \go -> do
    modify resetState
    return go

tickState :: SceneState -> SceneState
tickState (SceneState b autoUpdate) = SceneState (tick b) autoUpdate

toggleBoard :: Position -> SceneState -> SceneState
toggleBoard pos (SceneState b a) = SceneState (toggleAt pos b) a

resetState :: SceneState -> SceneState
resetState (SceneState b autoUpdate) = SceneState (initialBoard . reset $ b) autoUpdate

setConwayColorBehavior :: Position -> BehaviorType SceneState
setConwayColorBehavior pos go = do
    isLive <- gets (isLiveCellAt pos)
    let color = if isLive then white else red
    pushMessage (setFillColorMsg color) go

singleStepB :: KeyCode -> Behavior SceneState
singleStepB key = behaveOnKeyJustPressedB key $ \go -> do
    modify tickState
    return go

isLiveCellAt :: Position -> SceneState -> Bool
isLiveCellAt pos (SceneState s _) = isLive pos s

turnOnAutoUpdateB :: KeyCode -> Behavior SceneState
turnOnAutoUpdateB key = behaveOnKeyPressB key $ \go -> do
    (SceneState b _) <- get
    put (SceneState b True)
    return go

turnOffAutoUpdateB :: KeyCode -> Behavior SceneState
turnOffAutoUpdateB key = behaveOnKeyPressB key $ \go -> do
    (SceneState b _) <- get
    put (SceneState b False)
    return go


createInstructions :: MaybeT IO [GameObject SceneState]
createInstructions = do
    let pos = genPositions (Vec2f 420 30) (onY (+30))
    objs <- createLabels
    return (zipWith Pos.setPosition objs pos)

createLabels :: MaybeT IO [GameObject SceneState]
createLabels = do
    autoOn      <- newText "Auto ON: 'A'"
    autoOff     <- newText "Auto OFF: 'Z'"
    rstBoard    <- newText "Reset board: 'R'"
    singleStep  <- newText "Single step: 'S'"
    return [autoOn, autoOff, rstBoard, singleStep]
    where
        newText = createTextGO 15 white

genPositions :: Vec2f -> (Vec2f -> Vec2f) -> [Vec2f]
genPositions seed f = newPos : genPositions newPos f
    where
        newPos = f seed