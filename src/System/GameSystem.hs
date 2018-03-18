{-# LANGUAGE NamedFieldPuns #-}
module System.GameSystem
    ( startGame
    ) where


import SFML.Graphics.Types (RenderWindow)
import SFML.Graphics.RenderWindow (display, clearRenderWindow)
import SFML.Graphics.Color (black)

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import System.GameWorld (GameWorld (..), adoptChildren)
import System.GameScene (GameScene (..), updateGameScene)
import System.EventSystem (pollAllEvents, shouldCloseWindow)
import System.InputSnapshot (stepSnapshot)
import Input.Mouse (getMouseInput)
import GameEnv (GameEnvironment (..), updateGameEnv)
import Synchronizable
import Drawable
import NativeResource

startGame :: GameWorld -> GameScene a -> GameEnvironment -> IO ()
startGame world scene gameEnv = do
    endScene <- loop world scene gameEnv
    free endScene
    free world

loop :: GameWorld -> GameScene a -> GameEnvironment -> IO (GameScene a)
loop (GameWorld wnd) scene@GameScene { gameObjects } env = do 

    evts <- pollAllEvents wnd

    mouse <- getMouseInput wnd

    let newSnap = stepSnapshot (inputSnapshot env) evts
    let liveGameObjects = fromIntegral . length $ gameObjects
    newEnv <- updateGameEnv env mouse liveGameObjects newSnap

    unless (null evts) $ do
        putStrLn $ "These are the events: " ++ show evts
        putStrLn $ "This is the snapshot: " ++ show (inputSnapshot newEnv)

    updatedScene <- gameLoop scene newEnv
    updateScreen wnd updatedScene

    if any shouldCloseWindow evts then
        do  putStrLn ("Closing event: " ++ show evts)
            return updatedScene
    else
        loop (GameWorld wnd) updatedScene env


gameLoop :: GameScene a -> GameEnvironment -> IO (GameScene a)
gameLoop scene env = do
    threadDelay (10 * 10^3)
    (scene', orphanChildren) <- updateGameScene scene env
    return $ adoptChildren scene' orphanChildren

updateScreen :: RenderWindow -> GameScene a -> IO ()
updateScreen window scene = do
    synchronize scene
    clearRenderWindow window black
    draw window scene
    display window