module System.GameState
    ( GameState (..)
    , updateGameState

    ) where

import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT)

import System.GameWorld (GameWorld (..), adoptChildren)
import System.GameScene (GameScene (..), updateGameScene)
import System.EventSystem (pollAllEvents, shouldCloseWindow)
import System.Input.InputSnapshot (stepSnapshot)
import Input.Mouse (getMouseInput)
import GameEnv (GameEnvironment (..), updateGameEnv)

data GameState st = GS  { scene :: GameScene st
                        , env   :: GameEnvironment
                        }

updateGameState :: GameWorld -> GameState st -> MaybeT IO (GameState st)
updateGameState world gameState = do
    env' <- updateEnv world gameState
    updatedScene <- liftIO $ updateScene gameState
    return $ GS updatedScene env'

updateEnv :: GameWorld -> GameState st -> MaybeT IO GameEnvironment
updateEnv (GameWorld wnd) (GS scene env) = do
    evts <-  liftIO (pollAllEvents wnd)
    mouse <- liftIO (getMouseInput wnd)

    let newSnap = stepSnapshot (inputSnapshot env) evts
    let liveGameObjects = fromIntegral . length $ gameObjects scene
    newEnv <- liftIO $ updateGameEnv env mouse liveGameObjects newSnap

    if any shouldCloseWindow evts then do
        liftIO $ putStrLn ("Closing event: " ++ show evts)
        mzero
    else return newEnv

updateScene :: GameState a -> IO (GameScene a)
updateScene (GS scene env) = do
    (scene', orphanChildren) <- updateGameScene scene env
    return $ adoptChildren scene' orphanChildren
                        