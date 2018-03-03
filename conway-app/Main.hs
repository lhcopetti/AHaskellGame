{-# LANGUAGE CPP #-}
module Main where

import SFML.Window
import SFML.Graphics.RenderWindow
import SFML.Graphics.Color

import Control.Monad (liftM)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.State

import Physics.PhysicsWorld (createWorld, initPhysicsLibrary)
import Physics.PhysicsTypes (PhysicsWorld)
import System.Messaging.Messages.ShapeMessage
import System.Messaging.Handler.PushMessageHandler
import GameEnv (GameEnvironment (..), createGameEnv)
import GameObject.GameObject (GameObject)
import GameObject.AnyGameObject (AnyGameObject (..))
import GameObject.GameObjectTypes
import Component.Behavior.Behavior (setBehaviorT)
import ObjectsFactory
import GridGameObjectFactory
import System.GameSystem (startGame)
import System.GameWorld (GameWorld (..), GameScene (..))
import Vec2.Vec2Math
import Conway

defaultGravity :: Float
defaultGravity = 30

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
    let gameEnv = createGameEnv dimensions

    objs <- runMaybeT (createObjects gameEnv physicsWorld)
    case objs of 
        Nothing -> putStrLn "Error creating game objects"
        Just (b, objects) -> do
                            let n = setLive (1, 0) b
                                n' = setLive (1, 1) n
                                n'' = setLive (1, 2) n'
                            let anyObjs = map AGO objects
                            let world = GameWorld wnd
                            let scene = GameScene physicsWorld anyObjs n''
                            startGame world scene gameEnv 
                            putStrLn "This is the End!"

createObjects :: GameEnvironment -> PhysicsWorld -> MaybeT IO (ConwayWorld, [GameObject])
createObjects _ _ = do
    let gridSize = (3, 3)
    board <- newConwayWorld gridSize
    objs <- createGridObjects gridSize mkConwayCell
    logicGO <- createLogicGO (Behavior stepConway)
    return (board, logicGO : objs)

mkConwayCell :: Position -> MaybeT IO GameObject
mkConwayCell gpos@(x, y) = liftM updateBehavior createObject
    where
        fx = fromIntegral x
        fy = fromIntegral y
        pos = addVec2f (Vec2f 5 5) (Vec2f (fx * 50) (fy * 50))
        updateBehavior = setBehaviorT (setConwayColorBehavior gpos)
        createObject   = createSquareObject 40 white pos

stepConway :: BehaviorType
stepConway go = do
    modify tick
    return go

setConwayColorBehavior :: Position -> BehaviorType
setConwayColorBehavior pos go = do
    isLive <- gets (isLive pos)
    let color = if isLive then white else red
    pushMessage (setFillColorMsg color) go