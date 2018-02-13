{-# LANGUAGE NamedFieldPuns #-}
module System.GameSystem
    ( startGame
    ) where


import SFML.Graphics.RenderWindow (display, clearRenderWindow, destroy)
import SFML.Graphics.Color (black)

import Control.Monad (forM, forM_, unless)
import Control.Monad.Reader (runReader)
import Control.Concurrent (threadDelay)

import System.GameWorld (GameWorld (..), adoptChildren)
import System.EventSystem (pollAllEvents, shouldCloseWindow)
import System.InputSnapshot (createSnapshot)
import Input.Mouse (getMouseInput)
import GameEnv (GameEnvironment (..))
import GameObject.AnyGameObject (AnyGameObject, removeDeadAnyGameObjects, getChildrenAnyGameObjects, removeChildrenAnyGameObject, updatePhysicsAnyGameObjects)
import Updatable
import Synchronizable
import Drawable
import Component.Physics.Physics ()
import Physics.PhysicsWorld (stepWorld)

startGame :: GameWorld -> GameEnvironment -> IO ()
startGame world gameEnv = do
    loop world gameEnv
    destroy (window world)

drawObjects :: GameWorld -> IO ()
drawObjects GameWorld { window, gameObjects } = forM_ gameObjects (draw window)

synchronizeObjects :: GameWorld -> IO ()
synchronizeObjects GameWorld { gameObjects } = forM_ gameObjects synchronize

loop :: GameWorld -> GameEnvironment -> IO ()
loop world@(GameWorld _ wnd objs) env = do 

    evts <- pollAllEvents wnd

    mouse <- getMouseInput wnd

    let snapshot = createSnapshot evts
    let liveGameObjects = fromIntegral . length $ objs
    let newEnv = env    { input = mouse, 
                        countGOs = liveGameObjects,
                        inputSnapshot = snapshot
                        }

    unless (null evts) $ do 
        putStrLn $ "These are the events: " ++ show evts
        putStrLn $ "This is the snapshot: " ++ show (inputSnapshot newEnv)

    updatedWorld <- gameLoop world newEnv

    if any shouldCloseWindow evts then
        putStrLn ("Closing event: " ++ show evts)
    else
        loop updatedWorld env


gameLoop :: GameWorld -> GameEnvironment -> IO GameWorld
gameLoop world env = do
    threadDelay (10 * 10^3)

    (newWorld, orphanChildren) <- updateGameWorld world env
    updateScreen newWorld
    return $ adoptChildren newWorld orphanChildren

updateScreen :: GameWorld -> IO ()
updateScreen world @ GameWorld { window } = do
    clearRenderWindow window black
    synchronizeObjects world
    drawObjects world
    display window

updateGameWorld :: GameWorld -> GameEnvironment -> IO (GameWorld, [AnyGameObject])
updateGameWorld (GameWorld physicsWorld wnd objs) env = do
    objs' <- updatePhysicsAnyGameObjects objs
    let newObjs = runReader (forM objs' update) env
    childrenObj <- getChildrenAnyGameObjects newObjs
    newObjs' <- removeDeadAnyGameObjects newObjs
    let newObjs'' = map removeChildrenAnyGameObject newObjs'
    stepWorld (1 / 60) physicsWorld
    return (GameWorld physicsWorld wnd newObjs'', childrenObj)
