module System.GameState
    ( GameState (..)
    , updateGameState

    ) where

import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Time

import System.GameWorld (GameWorld (..), adoptChildren)
import System.GameScene (GameScene (..), updateGameScene)
import System.EventSystem (pollAllEvents, shouldCloseWindow)
import System.Input.InputSnapshot (stepSnapshot)
import Input.Mouse (getMouseInput)
import GameEnv (GameEnvironment (..), updateGameEnv)

data GameState st = GS  { scene :: GameScene st
                        , env   :: GameEnvironment
                        }

updateGameState :: GameWorld -> NominalDiffTime -> GameState st -> MaybeT IO (GameState st)
updateGameState world deltaTime gameState = do
    env' <- updateEnv world deltaTime gameState
    updatedScene <- liftIO $ updateScene gameState
    return $ GS updatedScene env'

updateEnv :: GameWorld -> NominalDiffTime -> GameState st -> MaybeT IO GameEnvironment
updateEnv (GameWorld wnd) deltaTime (GS scene env) = do
    evts <-  liftIO (pollAllEvents wnd)
    mouse <- liftIO (getMouseInput wnd)

    let newSnap = stepSnapshot (inputSnapshot env) evts
    let liveGameObjects = fromIntegral . length $ gameObjects scene
    newEnv <- liftIO $ updateGameEnv env deltaTime mouse liveGameObjects newSnap

    if any shouldCloseWindow evts then do
        liftIO $ putStrLn ("Closing event: " ++ show evts)
        mzero
    else return newEnv

updateScene :: GameState a -> IO (GameScene a)
updateScene (GS scene env) = do
    (scene', orphanChildren) <- updateGameScene scene env
    return $ adoptChildren scene' orphanChildren
                        