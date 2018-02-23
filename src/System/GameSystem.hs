{-# LANGUAGE NamedFieldPuns #-}
module System.GameSystem
    ( startGame
    ) where


import SFML.Graphics.Types (RenderWindow)
import SFML.Graphics.RenderWindow (display, clearRenderWindow, destroy)
import SFML.Graphics.Color (black)

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import System.GameWorld (GameWorld (..), adoptChildren)
import System.GameScene (GameScene (..), updateGameScene)
import System.EventSystem (pollAllEvents, shouldCloseWindow)
import System.InputSnapshot (createSnapshot)
import Input.Mouse (getMouseInput)
import GameEnv (GameEnvironment (..))
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
    updateScreen wnd updatedScene

    if any shouldCloseWindow evts then
        putStrLn ("Closing event: " ++ show evts)
    else
        loop (GameWorld wnd) updatedScene env


gameLoop :: GameScene -> GameEnvironment -> IO GameScene
gameLoop scene env = do
    threadDelay (10 * 10^3)
    (scene', orphanChildren) <- updateGameScene scene env
    return $ adoptChildren scene' orphanChildren

updateScreen :: RenderWindow -> GameScene -> IO ()
updateScreen window scene = do
    synchronize scene
    clearRenderWindow window black
    draw window scene
    display window