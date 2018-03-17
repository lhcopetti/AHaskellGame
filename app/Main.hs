{-# LANGUAGE CPP #-}
module Main where

import SFML.Window
import SFML.Graphics.RenderWindow

import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import System.Random (StdGen)
import Component.Draw.Animation.SpriteSheet (SpriteSheet (..), loadSpriteSheet)
import Physics.PhysicsWorld (createWorld, initPhysicsLibrary)
import Physics.PhysicsTypes (PhysicsWorld)

import GameEnv (GameEnvironment (..), createGameEnv)
import GameObject.GameObject (GameObject)
import GameObject.GameObjectTypes
import PrefabObjects.TriangleMouseFollower (createMouseFollowerEqTriangle)
import PrefabObjects.AnimatedBlueBird (createAnimatedBlueBird)
import PrefabObjects.BallInputAware (createBallInputAware)
import PrefabObjects.AnimatedRunningCat (createAnimatedRunningCat)
import PrefabObjects.AnimatedSpinningCoin (createSpinningCoin)
import PrefabObjects.MousePositionPrinter (mkMousePrinter)
import ObjectsFactory
import qualified Component.Position as Pos
import System.GameSystem (startGame)
import System.GameWorld (GameWorld (..), GameScene (..))
import Random.Random
import Conway
import Data.Time

import Paths_AHaskellGame

#define USE_RANDOM_GENERATOR

defaultGravity :: Float
defaultGravity = 30

main :: IO ()
main = do
    initPhysicsLibrary
    physicsWorld <- createWorld defaultGravity
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
    currTime <- getCurrentTime
    let gameEnv = createGameEnv dimensions currTime

    -- Initialize a Random Generator
#ifdef USE_RANDOM_GENERATOR
    gen <- newGenerator
#else
    gen <- newGeneratorFromString "1458194910 1"
#endif

    spriteSheetName <- getDataFileName "resources/sprites/blue-bird/blue-bird-10%-resized.png"
    spriteSheet <- runMaybeT $ loadSpriteSheet spriteSheetName (Ratio 2 4)
    case spriteSheet of
        (Just s) -> putStrLn $ "The number of sprites is: " ++ (show . length . sprites $ s)
        _ -> return ()

    objects  <- runMaybeT (createObjects gen gameEnv physicsWorld)
    case objects of 
        Nothing -> putStrLn "Error creating game objects"
        Just balls -> case newConwayWorld (5, 5) of 
                        Just _ -> do
                            let world = GameWorld wnd
                                scene = GameScene physicsWorld balls GVS
                            startGame world scene gameEnv
                            putStrLn "This is the End!"
                        Nothing -> do
                            putStrLn "Invalid board!" 
                            return ()


type BallCreation a = ReaderT GameEnvironment (StateT StdGen (MaybeT IO)) a

runBallCreation :: StdGen -> GameEnvironment -> BallCreation a -> MaybeT IO (a, StdGen)
runBallCreation gen env eval = runStateT (runReaderT eval env) gen

createObjects :: StdGen -> GameEnvironment -> PhysicsWorld -> MaybeT IO [GameObject GoVoidState]
createObjects gen env space = do 
    balls <- createGameBalls
    dots <- createDots
    triangles <- createTriangles
    hex <- createSimpleHexagon (Vec2f 200 200)
    eqT <- createMouseFollowerEqTriangle
    inputAware <- createBallInputAware (Vec2f 50 400)
    mousePointer <- createMousePositionCopier
    mousePrinter <- liftM (`Pos.setPosition` Vec2f 500 0) mkMousePrinter
    simpleText <- createSimpleText (Vec2f 100 100) "AHaskellGame"
    (randomObjects, _) <- runBallCreation gen env createRandomMiniBalls
    goCounter <- createLiveGameObjectCounter (Vec2f 20 20)
    willDieSoon <- createDeathByUpdates (Vec2f 400 400)
    willHitAndDie <- createDeathByHitsOnWall (Vec2f 200 200) (Vec2f 5.0 7.0)
    sprites <- createSprites
    behaveOnce <- createBehaveOnce (Vec2f 568 200)
    namedObjects <- createNamedMessagesDemo (Vec2f 468 300)
    behavesAll <- createUsesBehaveAll
    hipmunkBalls <- createPhysicsBalls space
    hipmunkLine <- createPhysicsLine 20.0 (Vec2f 0 400, Vec2f 640 400) space
    hLines <- createLines [(Vec2f 400 20, Vec2f 500 20, 1), (Vec2f 400 30, Vec2f 500 30, 3), (Vec2f 400 40, Vec2f 500 40, 5)]
    vLines <- createLines [
        (Vec2f 510 20, Vec2f 510 40, 1), 
        (Vec2f 520 20, Vec2f 520 40, 3), 
        (Vec2f 530 20, Vec2f 530 40, 5)]
    dLines <- createLines [
        (Vec2f 540 20, Vec2f 550 40, 1),
        (Vec2f 550 20, Vec2f 560 40, 3),
        (Vec2f 560 20, Vec2f 570 40, 5)]
    box1 <- createBox (Vec2f 40  350) 15 space
    box2 <- createBox (Vec2f 600 350) 15 space
    return (box1 : box2 : hipmunkLine : inputAware : behavesAll : namedObjects : behaveOnce : mousePrinter : willHitAndDie: willDieSoon : goCounter : simpleText : eqT : hex : mousePointer : balls ++ dots ++ triangles ++ randomObjects ++ sprites ++ hLines ++ vLines ++ dLines ++ hipmunkBalls)

createPhysicsBalls :: PhysicsWorld -> MaybeT IO [GameObject GoVoidState]
createPhysicsBalls physicsWorld = 
        let xs = take 5 [200, 250 ..]
            ys = take 5 [-100, -50, 0, 50, 100 ]
            xss = take 5 [225, 275 ..]
            positions = [ Vec2f x y | x <- xs, y <- ys]
            positionss = [Vec2f x (-200) | x <- xss]
            normal = flip createHipPhysicsBall 10
            big = flip createHipPhysicsBall 20
        in do
            patternBalls <- mapM (`normal`  physicsWorld) positions
            outOfPattern <- mapM (`big`     physicsWorld) positionss
            return (patternBalls ++ outOfPattern)

createSprites :: MaybeT IO [GameObject GoVoidState]
createSprites = do
    blueBird <- createSpriteFromFile "resources/sprites/blue-bird/blue-bird-0-resized.png" (Vec2f 400 100) (Vec2f 1.0 0)
    bird <- createAnimatedBlueBird (Vec2f 400 150) (Vec2f 1.0 0)
    cat <- createAnimatedRunningCat (Vec2f 400 200) (Vec2f 2.0 0)
    coin <- createSpinningCoin (Vec2f 36 300) (Vec2f 0 0)
    return [blueBird, bird, cat, coin]

createSpriteFromFile :: FilePath -> Vec2f -> Vec2f -> GameObjectCreation
createSpriteFromFile path pos vel = do
    systemPath <- liftIO $ getDataFileName path
    createSprite systemPath pos vel

createRandomMiniBalls :: BallCreation [GameObject GoVoidState]
createRandomMiniBalls = do
    pos <- createRandomPositions 5
    speed <- lift (createRandomSpeeds 8.0 10)
    sequence (ballCreationMiniBall <$> pos <*> speed)

ballCreationMiniBall :: Vec2f -> Vec2f -> BallCreation (GameObject GoVoidState)
ballCreationMiniBall pos vel = lift . lift $ createMiniBall pos vel

createGameBalls :: MaybeT IO [GameObject GoVoidState]
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

createDots :: MaybeT IO [GameObject GoVoidState]
createDots = do
    dot     <- createWhiteNoopBall (Vec2f 50 50)
    dot'    <- createWhiteNoopBall (Vec2f 150 150)
    dot''   <- createWhiteNoopBall (Vec2f 250 250)
    dot3    <- createWhiteNoopBall (Vec2f 350 350)
    return [dot, dot', dot'', dot3]

createTriangles :: MaybeT IO [GameObject GoVoidState]
createTriangles = do
    triangle <- createDeadManWalking (Vec2f 150 150)
    return [triangle]