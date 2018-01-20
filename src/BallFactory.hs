module BallFactory 
    where

import SFML.System.Vector2
import SFML.Graphics.Color

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createGameObject, createStaticGameObject, createStaticGameObjectB)
import GameObject.GameObject (GameObject)
import Component.Draw.Drawing (setOriginDrawing)
import Component.Draw.CircleDrawing (createCircle, createCenteredCircle)
import Component.Draw.RectangleDrawing (createSquare)
import Component.Draw.ConvexDrawing (createConvex)
import Component.Draw.HexagonDrawing (createHexagon)
import Component.Draw.TriangleDrawing (createEqTriangle)
import Component.Draw.TextDrawing (createEmptyText, createText)
import Component.Draw.CompositeDrawing (createComposite)
import Component.Draw.SpriteDrawing (createSpriteDrawing)
import Component.Behavior.Behaviors
import Vec2.Vec2Math (zero)

createMiniBall :: Vec2f -> Vec2f -> MaybeT IO GameObject
createMiniBall pos vel = do
    liftIO $ putStrLn $ "Creating mini ball at " ++ show pos
    let color = blue
    shape <- createCircle 2 color
    return (createGameObject shape encloseToBoxB pos vel)    

createBall :: Vec2f -> Vec2f -> MaybeT IO GameObject
createBall pos@(Vec2f x y) vel = do 
    liftIO $ putStrLn $ "Creating ball at " ++ show pos
    let color = blue
    shape <- createCircle 25 color
    return (createGameObject shape encloseToBoxB pos vel)

createRedBall :: Vec2f -> Vec2f -> MaybeT IO GameObject
createRedBall pos vel = do 
    liftIO $ putStrLn $ "Creating red ball at " ++ show pos
    drawComponent <- createCircle 10 red
    return (createGameObject drawComponent encloseToBoxB pos vel)

createYellowSquare :: Vec2f -> Vec2f -> MaybeT IO GameObject
createYellowSquare pos vel = do
    liftIO $ putStrLn $ "Creating yellow square at " ++ show pos
    drawComponent <- createSquare 5 yellow
    return (createGameObject drawComponent encloseByWrapAroundB pos vel)

createCyanTriangle :: Vec2f -> Vec2f -> MaybeT IO GameObject
createCyanTriangle pos vel = do
    liftIO $ putStrLn $ "Creating blue triangle at " ++ show pos
    drawComponent <- createConvex cyan [Vec2f 55 30, Vec2f 70 60, Vec2f 40 60]
    return (createGameObject drawComponent encloseToBoxB pos vel)

createMagentaWrapAroundBall :: Vec2f -> Vec2f -> MaybeT IO GameObject
createMagentaWrapAroundBall pos vel = do
    liftIO $ putStrLn $ "Creating magenta ball at " ++ show pos
    drawComponent <- createCircle 5 magenta
    return (createGameObject drawComponent encloseByWrapAroundB pos vel)

createWhiteNoopBall :: Vec2f -> MaybeT IO GameObject
createWhiteNoopBall pos = do
    liftIO $ putStrLn $ "Creating white noop ball " ++ show pos
    drawComponent <- createCircle 5 white
    return (createStaticGameObject drawComponent pos)

createDeadManWalking :: Vec2f -> MaybeT IO GameObject
createDeadManWalking pos = do
    liftIO $ putStrLn $ "Creating dead man walking noop triangle " ++ show pos
    drawComponent <- createConvex white [Vec2f 55 30, Vec2f 70 60, Vec2f 40 60]
    return (createGameObject drawComponent deadManWalkingB pos zero)

createSimpleHexagon :: Vec2f -> MaybeT IO GameObject
createSimpleHexagon pos = do
    liftIO $ putStrLn $ "Creating a simple hexagon " ++ show pos
    drawComponent <- createHexagon 25.0 white
    return (createGameObject drawComponent (rotateB 1.0) pos zero)

createSimpleEqTriangle :: Vec2f -> MaybeT IO GameObject
createSimpleEqTriangle pos = do
    liftIO $ putStrLn $ "Creating a simple eq triangle " ++ show pos
    let factor = 25.0
    drawComponent <- createEqTriangle factor white
    miniBall1 <- createCenteredCircle 5 yellow
    miniBall2 <- createCenteredCircle 5 yellow
    let triOriginX = sqrt 3 / 4 * factor + 5
    let triOriginY1 = 1/2 * factor + 5
    let triOriginY2 = -1/2 * factor + 5

    liftIO $ setOriginDrawing miniBall1 (Vec2f triOriginX triOriginY1)
    liftIO $ setOriginDrawing miniBall2 (Vec2f triOriginX triOriginY2)
    drw <- createComposite [drawComponent, miniBall1, miniBall2]
    return (createGameObject drw followPointingMouseB pos zero)


createMousePositionCopier :: MaybeT IO GameObject
createMousePositionCopier = do
    liftIO $ putStrLn "Creating mouse pointer"
    drawComponent <- createCenteredCircle 3 green
    return (createGameObject drawComponent mousePositionCopierB zero zero)

createMouseFollower :: Vec2f -> MaybeT IO GameObject
createMouseFollower pos = do
    liftIO $ putStrLn "Creating mouse follower"
    drawComponent <- createCenteredCircle 10 blue
    return (createGameObject drawComponent mouseFollowerB pos zero)

createSimpleText :: Vec2f -> String -> MaybeT IO GameObject
createSimpleText pos text = do
    liftIO $ putStrLn "Creating simple text"
    drawComponent <- createText 30 text
    return (createStaticGameObject drawComponent pos)

createLiveGameObjectCounter :: Vec2f -> MaybeT IO GameObject
createLiveGameObjectCounter pos = do
    liftIO $ putStrLn "Creating live GameObject counter"
    drawComponent <- createEmptyText 15
    let behavior = updatePromptForGOCountB "GameObjects"
    return (createStaticGameObjectB drawComponent pos behavior)

createDeathByUpdates :: Vec2f -> MaybeT IO GameObject
createDeathByUpdates pos = do
    liftIO $ putStrLn "Creating object that dies from updates"
    drawComponent <- createCenteredCircle 10 blue
    let behavior = deathByUpdatesB
    return (createStaticGameObjectB drawComponent pos behavior)

createDeathByHitsOnWall :: Vec2f -> Vec2f -> MaybeT IO GameObject
createDeathByHitsOnWall pos vel = do
    liftIO $ putStrLn "Creating object that dies from hitting on walls"
    drawComponent <- createCenteredCircle 15 green
    let behavior = deathByHitsOnWallB
    return (createGameObject drawComponent behavior pos vel)

createSprite :: FilePath -> Vec2f -> Vec2f -> MaybeT IO GameObject
createSprite path pos vel = do
    liftIO $ putStrLn ("Creating a Sprite GameObject from " ++ path)
    drawComponent <- createSpriteDrawing path
    let behavior = encloseByWrapAroundB
    return (createGameObject drawComponent behavior pos vel)