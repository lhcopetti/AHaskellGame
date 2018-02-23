{-# LANGUAGE NamedFieldPuns #-}
module System.GameSystem
    ( startGame
    ) where


import SFML.Graphics.Types (RenderWindow)
import SFML.Graphics.RenderWindow (display, clearRenderWindow, destroy)
import SFML.Graphics.Color (black)

import Control.Monad (forM_, unless)
import Control.Concurrent (threadDelay)

import System.GameWorld (GameWorld (..), GameScene (..), adoptChildren)
import System.EventSystem (pollAllEvents, shouldCloseWindow)
import System.InputSnapshot (createSnapshot)
import System.GameStepper (stepPhysics, stepGameObjects)
import Input.Mouse (getMouseInput)
import GameEnv (GameEnvironment (..))
import GameObject.AnyGameObject (AnyGameObject)
import Synchronizable
import Drawable

startGame :: GameWorld -> GameScene -> GameEnvironment -> IO ()
startGame world scene gameEnv = do
    loop world scene gameEnv
    destroy (window world)

drawObjects :: RenderWindow -> [AnyGameObject] -> IO ()
drawObjects window gameObjects = forM_ gameObjects (draw window)

synchronizeObjects :: GameScene -> IO ()
synchronizeObjects GameScene { gameObjects } = forM_ gameObjects synchronize

loop :: GameWorld -> GameScene -> GameEnvironment -> IO ()
loop (GameWorld wnd) scene@(GameScene _ objs) env = do 

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

    updatedScene <- gameLoop (GameWorld wnd) scene newEnv

    if any shouldCloseWindow evts then
        putStrLn ("Closing event: " ++ show evts)
    else
        loop (GameWorld wnd) updatedScene env


gameLoop :: GameWorld -> GameScene -> GameEnvironment -> IO GameScene
gameLoop world scene env = do
    threadDelay (10 * 10^3)

    (scene', orphanChildren) <- updateGameWorld scene env
    updateScreen world scene'
    return $ adoptChildren scene' orphanChildren

updateScreen :: GameWorld -> GameScene -> IO ()
updateScreen GameWorld { window } scene = do
    synchronizeObjects scene
    clearRenderWindow window black
    drawObjects window (gameObjects scene)
    display window

updateGameWorld :: GameScene -> GameEnvironment -> IO (GameScene, [AnyGameObject])
updateGameWorld (GameScene physicsWorld objs) env = do
    objs' <- stepPhysics (1 / 60) physicsWorld objs
    (newObjs, childrenObj) <- stepGameObjects env objs'
    return (GameScene physicsWorld newObjs, childrenObj)