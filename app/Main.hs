module Main where

import Lib
import SFML.Window
import Control.Monad (when, unless)
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

data GameWorld = GameWorld  { window :: RenderWindow
                            , customBall :: Ball
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

    customBall <- createBall (Vec2f 25 25) (Vec2f 4 4)

    case customBall of 
        Nothing -> putStrLn "Error creating custom ball."
        Just ball  -> do
            let world = GameWorld wnd ball
            loop world
            destroy wnd
            putStrLn "This is the End!"


shouldCloseWindow :: SFEvent -> Bool
shouldCloseWindow evt = (evt == SFEvtClosed) || (evt == SFEvtMouseButtonPressed {})

draw :: GameWorld -> IO ()
draw (GameWorld wnd ball) = drawBall wnd ball

loop :: GameWorld -> IO ()
loop all@(GameWorld wnd ball) = do 

    threadDelay (10 * 10^3)
    clearRenderWindow wnd black
    draw all
    display wnd

    dimensions@(Vec2u uWidth uHeight) <- getWindowSize wnd
    newCustomBall <- updateBall dimensions ball

    evt <- pollEvent wnd
    case evt of 
        Nothing -> loop (GameWorld wnd newCustomBall)
        (Just event) -> Control.Monad.unless (shouldCloseWindow event) $ loop (GameWorld wnd newCustomBall)