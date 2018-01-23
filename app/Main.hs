{-# LANGUAGE CPP #-}
module Main where

import SFML.Window
import SFML.Graphics.RenderWindow

import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class (liftIO)
import System.Random (StdGen)

import GameEnv (GameEnvironment (..), createGameEnv)
import GameObject.GameObject (GameObject)
import GameObject.AnyGameObject (AnyGameObject (..))
import GameObject.GameObjectTypes (GameObjectCreation)
import PrefabObjects.TriangleMouseFollower (createMouseFollowerEqTriangle)
import ObjectsFactory
import System.GameSystem (startGame)
import System.GameWorld (GameWorld (..))
import Random.Random

import Paths_AHaskellGame

#define USE_RANDOM_GENERATOR

main :: IO ()
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

createObjects :: StdGen -> GameEnvironment -> MaybeT IO [GameObject]
createObjects gen env = do 
    balls <- createGameBalls
    multiplier <- createMultiplier (Vec2f 5 5)
    dots <- createDots
    triangles <- createTriangles
    hex <- createSimpleHexagon (Vec2f 200 200)
    eqT <- createMouseFollowerEqTriangle
    mousePointer <- createMousePositionCopier
    mousePrinter <- createMousePositionPrinter (Vec2f 500 0)
    simpleText <- createSimpleText (Vec2f 100 100) "AHaskellGame"
    (randomObjects, _) <- runBallCreation gen env createRandomMiniBalls
    goCounter <- createLiveGameObjectCounter (Vec2f 20 20)
    willDieSoon <- createDeathByUpdates (Vec2f 400 400)
    willHitAndDie <- createDeathByHitsOnWall (Vec2f 200 200) (Vec2f 5.0 7.0)
    sprites <- createSprites
    behaveOnce <- createBehaveOnce (Vec2f 568 200)
    namedObjects <- createNamedMessagesDemo (Vec2f 468 300)
    return ( namedObjects: behaveOnce : multiplier : mousePrinter : willHitAndDie: willDieSoon : goCounter : simpleText : eqT : hex : mousePointer : balls ++ dots ++ triangles ++ randomObjects ++ sprites)

createSprites :: MaybeT IO [GameObject]
createSprites = do
    blueBird <- createSpriteFromFile "resources/sprites/blue-bird/blue-bird-0-resized.png" (Vec2f 400 100) (Vec2f 1.0 0)
    return [blueBird]

createSpriteFromFile :: FilePath -> Vec2f -> Vec2f -> GameObjectCreation
createSpriteFromFile path pos vel = do
    systemPath <- liftIO $ getDataFileName path
    createSprite systemPath pos vel

createRandomMiniBalls :: BallCreation [GameObject]
createRandomMiniBalls = do
    pos <- createRandomPositions 5
    speed <- lift (createRandomSpeeds 8.0 10)
    sequence (ballCreationMiniBall <$> pos <*> speed)

ballCreationMiniBall :: Vec2f -> Vec2f -> BallCreation GameObject
ballCreationMiniBall pos vel = lift . lift $ createMiniBall pos vel

createGameBalls :: MaybeT IO [GameObject]
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
    ball13 <- createMagentaWrapAroundBall (Vec2f 300 300) (Vec2f 2.5 4.5)
    ball14 <- createWhiteNoopBall (Vec2f 60 60)
    return [ball, ball2, ball3, ball4, ball5, ball6, ball7, ball8, ball9, ball13, ball14]

createDots :: MaybeT IO [GameObject]
createDots = do
    dot     <- createWhiteNoopBall (Vec2f 50 50)
    dot'    <- createWhiteNoopBall (Vec2f 150 150)
    dot''   <- createWhiteNoopBall (Vec2f 250 250)
    dot3    <- createWhiteNoopBall (Vec2f 350 350)
    return [dot, dot', dot'', dot3]

createTriangles :: MaybeT IO [GameObject]
createTriangles = do
    triangle <- createDeadManWalking (Vec2f 150 150)
    return [triangle]