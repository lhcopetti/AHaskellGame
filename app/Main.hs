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
                            , circle :: CircleShape
                            , customBall :: Ball
                            , direction :: Int
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

    myCircle <- createCircleShape
    case myCircle of 
        (Left ex) -> putStrLn $ "Error ocurred: " ++ show ex
        (Right circle) -> do
            setFillColor circle green
            setRadius circle 25

            customBall <- createBall (Vec2f 25 25) (Vec2f 4 4)

            case customBall of 
                Nothing -> putStrLn "Error creating custom ball."
                Just b  -> do
                    let world = GameWorld wnd circle b 1
                    loop world
                    destroy wnd
                    putStrLn "This is the End!"


shouldCloseWindow :: SFEvent -> Bool
shouldCloseWindow evt = (evt == SFEvtClosed) || (evt == SFEvtMouseButtonPressed {})

updateCircle :: CircleShape -> Vec2f -> Int -> IO Int
updateCircle circle (Vec2f width height) direction = do 
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

draw :: GameWorld -> IO ()
draw (GameWorld wnd circle ball dir) = do 
    drawCircle wnd circle Nothing
    drawBall wnd ball

loop :: GameWorld -> IO ()
loop all@(GameWorld wnd circle ball dir) = do 

    threadDelay (10 * 10^3)
    clearRenderWindow wnd black
    draw all
    display wnd

    dimensions@(Vec2u uWidth uHeight) <- getWindowSize wnd

    directionn <- updateCircle circle (Vec2f (fromIntegral uWidth) (fromIntegral uHeight)) dir
    newCustomBall <- updateBall dimensions ball

    evt <- pollEvent wnd
    case evt of 
        Nothing -> loop (GameWorld wnd circle newCustomBall directionn)
        (Just event) -> Control.Monad.unless (shouldCloseWindow event) $ loop (GameWorld wnd circle newCustomBall directionn)