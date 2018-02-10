{-# LANGUAGE CPP #-}
module Main where

import SFML.Window
import SFML.Graphics.RenderWindow

import qualified Physics.Hipmunk as H

import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class (liftIO)
import System.Random (StdGen)
import Component.Draw.Animation.SpriteSheet (SpriteSheet (..), loadSpriteSheet)
import Component.Physics.HipMunk.HipmunkWorld (createSpace)

import GameEnv (GameEnvironment (..), createGameEnv)
import GameObject.GameObject (GameObject)
import GameObject.AnyGameObject (AnyGameObject (..))
import GameObject.GameObjectTypes (GameObjectCreation, Ratio (..))
import PrefabObjects.TriangleMouseFollower (createMouseFollowerEqTriangle)
import PrefabObjects.AnimatedBlueBird (createAnimatedBlueBird)
import PrefabObjects.BallInputAware (createBallInputAware)
import PrefabObjects.AnimatedRunningCat (createAnimatedRunningCat)
import PrefabObjects.AnimatedSpinningCoin (createSpinningCoin)
import ObjectsFactory
import System.GameSystem (startGame)
import System.GameWorld (GameWorld (..))
import Random.Random
import qualified Data.StateVar as SV
import qualified SFML.Graphics as SF

import Paths_AHaskellGame

#define USE_RANDOM_GENERATOR

main :: IO ()
main = do
    H.initChipmunk
    space <- createSpace
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

    spriteSheetName <- getDataFileName "resources/sprites/blue-bird/blue-bird-10%-resized.png"
    spriteSheet <- runMaybeT $ loadSpriteSheet spriteSheetName (Ratio 2 4)
    case spriteSheet of
        (Just s) -> putStrLn $ "The number of sprites is: " ++ (show . length . sprites $ s)
        _ -> return ()

    objects  <- runMaybeT (createObjects gen gameEnv space)
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

createObjects :: StdGen -> GameEnvironment -> H.Space -> MaybeT IO [GameObject]
createObjects gen env space = do 
    balls <- createGameBalls
    dots <- createDots
    triangles <- createTriangles
    hex <- createSimpleHexagon (Vec2f 200 200)
    eqT <- createMouseFollowerEqTriangle
    inputAware <- createBallInputAware (Vec2f 50 400)
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
    behavesAll <- createUsesBehaveAll
    hipmunkObject <- createHipPhysicsBall (Vec2f 391 143) space
    return ( hipmunkObject : inputAware : behavesAll : namedObjects : behaveOnce : mousePrinter : willHitAndDie: willDieSoon : goCounter : simpleText : eqT : hex : mousePointer : balls ++ dots ++ triangles ++ randomObjects ++ sprites)

createSprites :: MaybeT IO [GameObject]
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

createRandomMiniBalls :: BallCreation [GameObject]
createRandomMiniBalls = do
    pos <- createRandomPositions 5
    speed <- lift (createRandomSpeeds 8.0 10)
    sequence (ballCreationMiniBall <$> pos <*> speed)

ballCreationMiniBall :: Vec2f -> Vec2f -> BallCreation GameObject
ballCreationMiniBall pos vel = lift . lift $ createMiniBall pos vel

createPhysicsObject :: H.Space -> H.Position -> IO (H.Shape, (SF.RenderWindow -> IO (), IO ()))
createPhysicsObject space pos = do
    let mass   = 20
        radius = 20
        t = H.Circle radius
    b <- H.newBody mass $ H.momentForCircle mass (0, radius) 0
    s <- H.newShape b t 0
    ----
    H.position   b SV.$= pos
    H.friction   s SV.$= 0.5
    H.elasticity s SV.$= 0.9

    H.spaceAdd space b
    H.spaceAdd space s
    let draw = do
            drawMyShape s t
    let remove = do
            H.spaceRemove space b
            H.spaceRemove space s
    return (s, (draw, remove))

-- | Draws a shape (assuming zero offset)
drawMyShape :: H.Shape -> H.ShapeType -> SF.RenderWindow -> IO ()
drawMyShape shape (H.Circle _) _ = do
  H.Vector _ _ <- SV.get $ H.position $ H.body shape
  _          <- SV.get $ H.angle    $ H.body shape

  undefined
drawMyShape _ _ _ = undefined

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