module Main where

import Lib
import SFML.Window
import Control.Monad (forM_, forM, mzero)
import SFML.Utils
import SFML.Graphics.CircleShape
import SFML.Graphics.Color
import SFML.Graphics.SFRenderTarget
import SFML.Graphics.RenderWindow
import SFML.Graphics.Types
import Control.Concurrent
import Foreign.Marshal.Utils
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.IO.Class

import GameObject.AnyGameObject
import GameEnv (GameEnvironment(..))
import GameObject.Ball
import GameObject.Square
import GameObject.Dot

data GameWorld = GameWorld  { window :: RenderWindow
                            , gameObjects :: [AnyGameObject]
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
        Just (balls, squares, dots) -> do
            let anyBalls = map AGO balls
            let anySquares = map AGO squares
            let anyDots = map AGO dots
            let world = GameWorld wnd (anyBalls ++ anySquares ++ anyDots)
            loop world gameEnv
            destroy wnd
            putStrLn "This is the End!"

createObjects :: MaybeT IO ([Ball], [Square], [Dot])
createObjects = do 
    balls <- createGameBalls
    squares <- createGameSquares
    dots <- createDots
    return (balls, squares, dots)

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

createDots :: MaybeT IO [Dot]
createDots = do
    dot     <- createDot (Vec2f 50 50)
    dot'    <- createDot (Vec2f 150 150)
    dot''   <- createDot (Vec2f 250 250)
    dot3    <- createDot (Vec2f 350 350)
    return [dot, dot', dot'', dot3]

shouldCloseWindow :: SFEvent -> Bool
shouldCloseWindow SFEvtClosed                   = True
shouldCloseWindow SFEvtMouseButtonPressed {}    = True
shouldCloseWindow _                             = False

drawObjects :: GameWorld -> IO ()
drawObjects (GameWorld wnd objs) = forM_ objs (drawAnyGameObject wnd)

loop :: GameWorld -> GameEnvironment -> IO ()
loop all@(GameWorld wnd objs) env = do 

    updatedWorld <- gameLoop all env

    evt <- runMaybeT (eventLoop wnd)
    case evt of 
        Nothing -> loop updatedWorld env
        (Just event) -> putStrLn ("Closing event: " ++ show event)


gameLoop :: GameWorld -> GameEnvironment -> IO GameWorld
gameLoop all@(GameWorld wnd objs) env = do 
    threadDelay (10 * 10^3)
    clearRenderWindow wnd black

    newObjs <- runReaderT (forM objs updateAnyGameObject) env

    drawObjects all
    display wnd

    return (GameWorld wnd newObjs)

eventLoop :: RenderWindow -> MaybeT IO SFEvent
eventLoop window = do 
    evt <- pollEventT window
    if shouldCloseWindow evt then return evt else mzero

pollEventT :: RenderWindow -> MaybeT IO SFEvent
pollEventT = MaybeT . pollEvent