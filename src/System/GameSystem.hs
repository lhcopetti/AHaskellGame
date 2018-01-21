{-# LANGUAGE NamedFieldPuns #-}
module System.GameSystem
    ( startGame
    ) where


import SFML.Graphics.RenderWindow (display, clearRenderWindow, destroy)
import SFML.Graphics.Color (black)

import Control.Monad (forM, forM_)
import Control.Monad.Reader (runReader)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Concurrent (threadDelay)

import System.GameWorld (GameWorld (..), adoptChildren)
import System.EventSystem (pollClosingEvent)
import Input.Mouse (MouseInput (..), getMouseInput)
import GameEnv (GameEnvironment (..))
import GameObject.AnyGameObject (AnyGameObject, updateAnyGameObject, drawAnyGameObject, removeDeadAnyGameObjects, synchronizeGameObject, getChildrenAnyGameObjects, removeChildrenAnyGameObject)

startGame :: GameWorld -> GameEnvironment -> IO ()
startGame world gameEnv = do
    loop world gameEnv
    destroy (window world)

drawObjects :: GameWorld -> IO ()
drawObjects (GameWorld wnd objs) = forM_ objs (drawAnyGameObject wnd)

synchronizeObjects :: GameWorld -> IO ()
synchronizeObjects (GameWorld wnd objs) = forM_ objs synchronizeGameObject

loop :: GameWorld -> GameEnvironment -> IO ()
loop all@(GameWorld wnd objs) env = do 

    mouse <- getMouseInput wnd
    let liveGameObjects = fromIntegral . length $ objs
    updatedWorld <- gameLoop all env { input = mouse, countGOs = liveGameObjects }

    evt <- runMaybeT (pollClosingEvent wnd)
    case evt of 
        Nothing -> loop updatedWorld env
        (Just event) -> putStrLn ("Closing event: " ++ show event)


gameLoop :: GameWorld -> GameEnvironment -> IO GameWorld
gameLoop all@(GameWorld wnd objs) env = do 
    threadDelay (10 * 10^3)
    clearRenderWindow wnd black

    (newWorld, orphanChildren) <- updateGameWorld all env
    updateScreen newWorld
    return $ adoptChildren newWorld orphanChildren

updateScreen :: GameWorld -> IO ()
updateScreen world @ GameWorld { window } = do
    synchronizeObjects world
    drawObjects world
    display window

updateGameWorld :: GameWorld -> GameEnvironment -> IO (GameWorld, [AnyGameObject])
updateGameWorld (GameWorld wnd objs) env = do
    let newObjs = runReader (forM objs updateAnyGameObject) env
    childrenObj <- getChildrenAnyGameObjects newObjs
    newObjs' <- removeDeadAnyGameObjects newObjs
    let newObjs'' = map removeChildrenAnyGameObject newObjs'
    return (GameWorld wnd newObjs'', childrenObj)