module Main where

import Lib
import SFML.Window
import Control.Monad (when, unless, forM_, forM, guard)
import Data.Maybe (isNothing)
import SFML.Utils
import SFML.Graphics.CircleShape
import SFML.Graphics.Color
import SFML.Graphics.SFRenderTarget
import SFML.Graphics.RenderWindow
import SFML.Graphics.Types
import Control.Concurrent
import Foreign.Marshal.Utils
import Ball
import Square
import GameEnv (GameEnvironment(..))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.IO.Class

data GameWorld = GameWorld  { window :: RenderWindow
                            , balls :: [Ball]
                            , squares :: [Square]
                            }

main = do
    desktopMode <- getDesktopMode
    fsModes <- getFullscreenModes

    putStrLn $ "Current desktop Mode is: " ++ show desktopMode
    putStrLn ""
    putStrLn "Fullscreen modes: "
    putStrLn ""

    mapM_ (\m -> print m >> putStrLn "") fsModes

    let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
    wnd <- createRenderWindow (VideoMode 640 480 32) "SFML Haskell Demo" [SFDefaultStyle] ctxSettings
    
    -- Game Environment initialization
    dimensions <- getWindowSize wnd
    let gameEnv = GameEnvironment dimensions 0


    createdBalls <- runMaybeT createObjects
    case createdBalls of 
        Nothing -> putStrLn "Error creating game objects"
        Just (balls, squares) -> do
            let world = GameWorld wnd balls squares
            loop world gameEnv
            destroy wnd
            putStrLn "This is the End!"

createObjects :: MaybeT IO ([Ball], [Square])
createObjects = do 
    balls <- createGameBalls
    squares <- createGameSquares
    return (balls, squares)

createGameSquares :: MaybeT IO [Square]
createGameSquares = do
    sq <- createSquare (Vec2f 25 25) (Vec2f 2 2)
    sq' <- createSquare (Vec2f 15 15) (Vec2f 1 0)
    sq'' <- createSquare (Vec2f 150 150) (Vec2f 1 1)
    return [sq, sq', sq'']

createGameBalls :: MaybeT IO [Ball]
createGameBalls = do
    ball <- createBall (Vec2f 25 25) (Vec2f 4 4)
    ball2 <- createBall (Vec2f 15 15) (Vec2f 2 0)
    ball3 <- createBall (Vec2f 150 150) (Vec2f 1 3)
    return [ball, ball2, ball3]


shouldCloseWindow :: SFEvent -> Bool
shouldCloseWindow evt = (evt == SFEvtClosed) || (evt == SFEvtMouseButtonPressed {})

drawObjects :: GameWorld -> IO ()
drawObjects (GameWorld wnd balls squares) = do 
    forM_ balls (draw wnd)
    forM_ squares (draw wnd)

loop :: GameWorld -> GameEnvironment -> IO ()
loop all@(GameWorld wnd balls squares) env = do 

    threadDelay (10 * 10^3)
    clearRenderWindow wnd black
    drawObjects all
    display wnd

    newBalls <- runReaderT (forM balls update) env
    newSquares <- runReaderT (forM squares update) env

    evt <- pollEvent wnd
    case evt of 
        Nothing -> loop (GameWorld wnd newBalls newSquares) env
        (Just event) -> Control.Monad.unless (shouldCloseWindow event) $ loop (GameWorld wnd newBalls newSquares) env