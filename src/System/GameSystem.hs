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

loop :: GameWorld -> GameScene -> GameEnvironment -> IO ()
loop (GameWorld wnd) scene@(GameScene _ objs) env = do 

    evts <- pollAllEvents wnd

    mouse <- getMouseInput wnd

    let snapshot = createSnapshot evts
    let liveGameObjects = fromIntegral . length $ objs
    let newEnv = env    { input = mouse
                        , countGOs = liveGameObjects
                        , inputSnapshot = snapshot
                        }

    unless (null evts) $ do 
        putStrLn $ "These are the events: " ++ show evts
        putStrLn $ "This is the snapshot: " ++ show (inputSnapshot newEnv)

    updatedScene <- gameLoop scene newEnv
    updateScreen wnd (gameObjects updatedScene)

    if any shouldCloseWindow evts then
        putStrLn ("Closing event: " ++ show evts)
    else
        loop (GameWorld wnd) updatedScene env


gameLoop :: GameScene -> GameEnvironment -> IO GameScene
gameLoop scene env = do
    threadDelay (10 * 10^3)
    (scene', orphanChildren) <- updateGameWorld scene env
    return $ adoptChildren scene' orphanChildren

updateGameWorld :: GameScene -> GameEnvironment -> IO (GameScene, [AnyGameObject])
updateGameWorld (GameScene physicsWorld objs) env = do
    objs' <- stepPhysics (1 / 60) physicsWorld objs
    (newObjs, childrenObj) <- stepGameObjects env objs'
    return (GameScene physicsWorld newObjs, childrenObj)


updateScreen :: RenderWindow -> [AnyGameObject] -> IO ()
updateScreen window gameObjects = do
    synchronizeObjects gameObjects

    clearRenderWindow window black
    drawObjects window gameObjects
    display window


drawObjects :: RenderWindow -> [AnyGameObject] -> IO ()
drawObjects window gameObjects = forM_ gameObjects (draw window)

synchronizeObjects :: [AnyGameObject] -> IO ()
synchronizeObjects gameObjects = forM_ gameObjects synchronize