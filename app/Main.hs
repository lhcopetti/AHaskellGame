{-# LANGUAGE CPP #-}
module Main where

import SFML.Window
import SFML.Graphics.SFRenderTarget
import SFML.Graphics.RenderWindow

import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import System.Random (StdGen)
import Data.Functor.Identity

import GameEnv (GameEnvironment (..), createGameEnv)
import GameObject.Ball (Ball)
import GameObject.AnyGameObject (AnyGameObject (..))
import BallFactory
import System.GameSystem (startGame)
import System.GameWorld (GameWorld (..))
import Random.Random
import Random.RandomState

#define USE_RANDOM_GENERATOR

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
    let gameEnv = createGameEnv dimensions

    -- Initialize a Random Generator
#ifdef USE_RANDOM_GENERATOR
    gen <- newGenerator
#else
    gen <- newGeneratorFromString "1458194910 1"
#endif

    objects  <- runMaybeT (createObjects gen gameEnv)
    case objects of 
        Nothing -> putStrLn "Error creating game objects"
        Just balls -> do
            let anyBalls = map AGO balls
            let world = GameWorld wnd anyBalls
            startGame world gameEnv
            putStrLn "This is the End!"


type BallCreation a = ReaderT GameEnvironment (StateT StdGen (MaybeT IO)) a

runBallCreation :: StdGen -> GameEnvironment -> BallCreation a -> MaybeT IO (a, StdGen)
runBallCreation gen env eval = runStateT (runReaderT eval env) gen

createObjects :: StdGen -> GameEnvironment -> MaybeT IO [Ball]
createObjects gen env = do 
    balls <- createGameBalls
    dots <- createDots
    triangles <- createTriangles
    mousePointer <- createMousePointer
    mouseFollowers <- createMouseFollowers
    (randomObjects, _) <- runBallCreation gen env createRandomMiniBalls
    return (mousePointer : mouseFollowers ++ balls ++ dots ++ triangles ++ randomObjects)

createRandomMiniBalls :: BallCreation [Ball]
createRandomMiniBalls = do
    position <- createRandomPositions 5
    speed <- lift (createRandomSpeeds 8.0 10)
    sequence (ballCreationMiniBall <$> position <*> speed)

ballCreationMiniBall :: Vec2f -> Vec2f -> BallCreation Ball
ballCreationMiniBall pos vel = lift . lift $ createMiniBall pos vel


createMouseFollowers :: MaybeT IO [Ball]
createMouseFollowers = do
    m <- createMouseFollower    (Vec2f 0.0 100.0)
    m' <- createMouseFollower   (Vec2f 0.0 250.0)    
    m'' <- createMouseFollower  (Vec2f 0.0 500.0)  
    return [m, m', m'']

createGameBalls :: MaybeT IO [Ball]
createGameBalls = do
    ball <- createBall (Vec2f 25 25) (Vec2f 4 4)
    ball2 <- createBall (Vec2f 15 15) (Vec2f 2 0)
    ball3 <- createBall (Vec2f 150 150) (Vec2f 1 3)
    ball4 <- createRedBall (Vec2f 300 150) (Vec2f 4 3)
    ball5 <- createRedBall (Vec2f 350 150) (Vec2f 1 (-3))
    ball6 <- createRedBall (Vec2f 400 150) (Vec2f (-1) 2)
    ball7 <- createYellowSquare (Vec2f 100 100) (Vec2f 2 3)
    ball8 <- createYellowSquare (Vec2f 200 200) (Vec2f 2 (-3))
    ball9 <- createYellowSquare (Vec2f 300 300) (Vec2f 2 (-5))
    ball10 <- createCyanTriangle (Vec2f 250 300) (Vec2f 1 2)
    ball11 <- createCyanTriangle (Vec2f 150 200) (Vec2f 1 (-3))
    ball12 <- createCyanTriangle (Vec2f 50 100) (Vec2f (-1) 1)
    ball13 <- createMagentaWrapAroundBall (Vec2f 300 300) (Vec2f 2.5 4.5)
    ball14 <- createWhiteNoopBall (Vec2f 60 60)
    return [ball, ball2, ball3, ball4, ball5, ball6, ball7, ball8, ball9, ball10, ball11, ball12, ball13, ball14]

createDots :: MaybeT IO [Ball]
createDots = do
    dot     <- createWhiteNoopBall (Vec2f 50 50)
    dot'    <- createWhiteNoopBall (Vec2f 150 150)
    dot''   <- createWhiteNoopBall (Vec2f 250 250)
    dot3    <- createWhiteNoopBall (Vec2f 350 350)
    return [dot, dot', dot'', dot3]

createTriangles :: MaybeT IO [Ball]
createTriangles = do
    triangle <- createDeadManWalking (Vec2f 150 150)
    return [triangle]