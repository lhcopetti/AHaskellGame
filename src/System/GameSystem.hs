{-# LANGUAGE NamedFieldPuns #-}
module System.GameSystem
    ( startGame
    ) where


import SFML.Graphics.RenderWindow (display, clearRenderWindow, destroy)
import SFML.Graphics.Color (black)

import Control.Monad (forM, forM_, when)
import Control.Monad.Reader (runReader)
import Control.Concurrent (threadDelay)

import System.GameWorld (GameWorld (..), adoptChildren)
import System.EventSystem (pollAllEvents, shouldCloseWindow)
import System.InputSnapshot (createSnapshot)
import Input.Mouse (getMouseInput)
import GameEnv (GameEnvironment (..))
import GameObject.AnyGameObject (AnyGameObject, updateAnyGameObject, drawAnyGameObject, removeDeadAnyGameObjects, synchronizeGameObject, getChildrenAnyGameObjects, removeChildrenAnyGameObject, updatePhysicsAnyGameObjects)
import Component.Physics.Physics ()

startGame :: GameWorld -> GameEnvironment -> IO ()
startGame world gameEnv = do
    loop world gameEnv
    destroy (window world)

drawObjects :: GameWorld -> IO ()
drawObjects (GameWorld wnd objs) = forM_ objs (drawAnyGameObject wnd)

synchronizeObjects :: GameWorld -> IO ()
synchronizeObjects (GameWorld _ objs) = forM_ objs synchronizeGameObject

loop :: GameWorld -> GameEnvironment -> IO ()
loop world@(GameWorld wnd objs) env = do 

    evts <- pollAllEvents wnd

    mouse <- getMouseInput wnd

    let snapshot = createSnapshot evts
    let liveGameObjects = fromIntegral . length $ objs
    let newEnv = env    { input = mouse, 
                        countGOs = liveGameObjects,
                        inputSnapshot = snapshot
                        }

    when (length evts > 0) (do 
        putStrLn $ "These are the events: " ++ (show evts)
        putStrLn $ "This is the snapshot: " ++ (show (inputSnapshot newEnv)))

    updatedWorld <- gameLoop world newEnv

    if any shouldCloseWindow evts then
        putStrLn ("Closing event: " ++ show evts)
    else
        loop updatedWorld env


gameLoop :: GameWorld -> GameEnvironment -> IO GameWorld
gameLoop world@(GameWorld wnd _) env = do
    threadDelay (10 * 10^3)
    clearRenderWindow wnd black

    (newWorld, orphanChildren) <- updateGameWorld world env
    updateScreen newWorld
    return $ adoptChildren newWorld orphanChildren

updateScreen :: GameWorld -> IO ()
updateScreen world @ GameWorld { window } = do
    synchronizeObjects world
    drawObjects world
    display window

updateGameWorld :: GameWorld -> GameEnvironment -> IO (GameWorld, [AnyGameObject])
updateGameWorld (GameWorld wnd objs) env = do
    objs' <- updatePhysicsAnyGameObjects objs
    let newObjs = runReader (forM objs' updateAnyGameObject) env
    childrenObj <- getChildrenAnyGameObjects newObjs
    newObjs' <- removeDeadAnyGameObjects newObjs
    let newObjs'' = map removeChildrenAnyGameObject newObjs'
    return (GameWorld wnd newObjs'', childrenObj)