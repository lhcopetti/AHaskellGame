module Main where

import Lib
import SFML.Window
import Control.Monad (when, unless)
import Data.Maybe (isNothing)
import SFML.Utils
import SFML.Graphics.CircleShape
import SFML.Graphics.Color
import SFML.Graphics.SFDrawable (draw)
import SFML.Graphics.SFRenderTarget
import SFML.Graphics.RenderWindow
import SFML.Graphics.Types
import Control.Concurrent
import Foreign.Marshal.Utils

data GameWorld = GameWorld { window :: RenderWindow
                           , circle :: CircleShape
                           , direction :: Int
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

    myCircle <- createCircleShape
    case myCircle of 
        (Left ex) -> putStrLn $ "Error ocurred: " ++ show ex
        (Right circle) -> do
            setFillColor circle green
            setRadius circle 25

            let world = GameWorld wnd circle 1

            loop world
            destroy wnd
            putStrLn "This is the End!"


shouldCloseWindow :: SFEvent -> Bool
shouldCloseWindow evt = (evt == SFEvtClosed) || (evt == SFEvtMouseButtonPressed {})

updateBall :: CircleShape -> Vec2f -> Int -> IO Int
updateBall circle (Vec2f width height) direction = do 
    let speed = 1
    (Vec2f x y) <- getPosition circle

    if direction > 0 then 
        setPosition circle (Vec2f (x + 1) y)
    else
        setPosition circle (Vec2f (x - 1) y)

    (Vec2f xx yy) <- getPosition circle
    if xx > width || xx < 0 then 
        return (direction * (-1))
    else 
        return direction

loop :: GameWorld -> IO ()
loop all@(GameWorld wnd circle dir) = do 

    threadDelay (10 * 10^3)
    clearRenderWindow wnd black
    drawCircle wnd circle Nothing
    display wnd

    (Vec2u uWidth uHeight) <- getWindowSize wnd

    directionn <- updateBall circle (Vec2f (fromIntegral uWidth) (fromIntegral uHeight)) dir

    evt <- pollEvent wnd
    case evt of 
        Nothing -> loop (GameWorld wnd circle directionn)
        (Just event) -> Control.Monad.unless (shouldCloseWindow event) $ loop (GameWorld wnd circle directionn)