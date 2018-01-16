module System.GameSystem
    ( startGame
    ) where


import SFML.Graphics.RenderWindow (display, clearRenderWindow, destroy)
import SFML.Graphics.Color (black)

import Control.Monad (forM, forM_)
import Control.Monad.Reader (runReader)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Concurrent (threadDelay)

import System.GameWorld (GameWorld (..))
import System.EventSystem (pollClosingEvent)
import Input.Mouse (MouseInput (..), getMouseInput)
import GameEnv (GameEnvironment (..))
import GameObject.AnyGameObject (updateAnyGameObject, drawAnyGameObject, removeDeadAnyGameObjects, synchronizeGameObject)

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
    updatedWorld <- gameLoop all env { input = mouse }

    evt <- runMaybeT (pollClosingEvent wnd)
    case evt of 
        Nothing -> loop updatedWorld env
        (Just event) -> putStrLn ("Closing event: " ++ show event)


gameLoop :: GameWorld -> GameEnvironment -> IO GameWorld
gameLoop all@(GameWorld wnd objs) env = do 
    threadDelay (10 * 10^3)
    clearRenderWindow wnd black

    let newObjs = runReader (forM objs updateAnyGameObject) env
    newObjs' <- removeDeadAnyGameObjects newObjs

    let newWorld = GameWorld wnd newObjs'

    synchronizeObjects newWorld
    drawObjects newWorld
    display wnd

    return newWorld