{-# LANGUAGE NamedFieldPuns #-}
module System.GameSystem
    ( startGame
    ) where


import SFML.Graphics.RenderWindow (display, clearRenderWindow)
import SFML.Graphics.Color (black)

import Control.Monad (foldM)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Time

import System.GameWorld (GameWorld (..))
import System.GameState (GameState (..), updateGameState)
import GameObject.GameObjectTypes (GameScene (..))
import GameEnv (GameEnvironment (..))
import Synchronizable
import Drawable
import NativeResource

startGame :: GameWorld -> GameScene a -> GameEnvironment -> IO ()
startGame world scene gameEnv = do
    let state = GS scene gameEnv
    time <- getCurrentTime
    endScene <- loop world state time 0
    free endScene
    free world

deltaTime :: NominalDiffTime
deltaTime = 0.01

loop :: GameWorld -> GameState st -> UTCTime -> NominalDiffTime -> IO (GameScene st)
loop world gameState currentTime acc = do 

    newTime <- getCurrentTime

    let frameTime = diffUTCTime newTime currentTime
        acc' = acc + frameTime
        numUpdates = floor (acc' / deltaTime) :: Int

    gameState' <- update world gameState numUpdates

    case gameState' of 
        Nothing -> return (scene gameState)
        Just newState -> do
            render world newState
            loop world newState newTime (acc' - realToFrac numUpdates * deltaTime)


update :: GameWorld -> GameState st -> Int -> IO (Maybe (GameState st))
update world st num = runMaybeT $ foldM (flip ($)) st updateFunctions
    where
        updateFunctions = replicate num (updateGameState world deltaTime)

render :: GameWorld -> GameState a -> IO ()
render (GameWorld wnd) (GS scene _) = do
    synchronize scene
    clearRenderWindow wnd black
    draw wnd scene
    display wnd