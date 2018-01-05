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
import GameEnv (GameEnvironment(..))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.IO.Class

data GameWorld = GameWorld  { window :: RenderWindow
                            , balls :: [Ball]
                            }

main = do
    printSFML
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


    createdBalls <- runMaybeT createGameBalls
    case createdBalls of 
        Nothing -> putStrLn "Error creating custom balls"
        Just balls -> do
            let world = GameWorld wnd balls
            loop world gameEnv
            destroy wnd
            putStrLn "This is the End!"

createGameBalls :: MaybeT IO [Ball]
createGameBalls = do
    ball <- createBall (Vec2f 25 25) (Vec2f 4 4)
    ball2 <- createBall (Vec2f 15 15) (Vec2f 2 0)
    ball3 <- createBall (Vec2f 150 150) (Vec2f 1 3)
    return [ball, ball2, ball3]


shouldCloseWindow :: SFEvent -> Bool
shouldCloseWindow evt = (evt == SFEvtClosed) || (evt == SFEvtMouseButtonPressed {})

draw :: GameWorld -> IO ()
draw (GameWorld wnd balls) = forM_ balls (drawBall wnd)

loop :: GameWorld -> GameEnvironment -> IO ()
loop all@(GameWorld wnd balls) env = do 

    threadDelay (10 * 10^3)
    clearRenderWindow wnd black
    draw all
    display wnd

    newBalls <- runReaderT (forM balls updateWithEnv) env

    evt <- pollEvent wnd
    case evt of 
        Nothing -> loop (GameWorld wnd newBalls) env
        (Just event) -> Control.Monad.unless (shouldCloseWindow event) $ loop (GameWorld wnd newBalls) env