module ObjectsFactory
    where

import SFML.System.Vector2
import SFML.Graphics.Color

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createGameObject, createGameObjectWithChildren, createStaticGameObject, createStaticGameObjectB)
import GameObject.GameObject (GameObject)
import GameObject.GameObjectTypes (GameObjectCreation)
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

createMiniBall :: Vec2f -> Vec2f -> GameObjectCreation
createMiniBall pos vel = do
    liftIO $ putStrLn $ "Creating mini ball at " ++ show pos
    let color = blue
    shape <- createCircle 2 color
    return (createGameObject shape encloseToBoxB pos vel)    

createBall :: Vec2f -> Vec2f -> GameObjectCreation
createBall pos@(Vec2f x y) vel = do 
    liftIO $ putStrLn $ "Creating ball at " ++ show pos
    let color = blue
    shape <- createCircle 25 color
    return (createGameObject shape encloseToBoxB pos vel)

createRedBall :: Vec2f -> Vec2f -> GameObjectCreation
createRedBall pos vel = do 
    liftIO $ putStrLn $ "Creating red ball at " ++ show pos
    drawComponent <- createCircle 10 red
    return (createGameObject drawComponent encloseToBoxB pos vel)

createYellowSquare :: Vec2f -> Vec2f -> GameObjectCreation
createYellowSquare pos vel = do
    liftIO $ putStrLn $ "Creating yellow square at " ++ show pos
    drawComponent <- createSquare 5 yellow
    return (createGameObject drawComponent encloseByWrapAroundB pos vel)

createCyanTriangle :: Vec2f -> Vec2f -> GameObjectCreation
createCyanTriangle pos vel = do
    liftIO $ putStrLn $ "Creating blue triangle at " ++ show pos
    drawComponent <- createConvex cyan [Vec2f 55 30, Vec2f 70 60, Vec2f 40 60]
    return (createGameObject drawComponent encloseToBoxB pos vel)

createMagentaWrapAroundBall :: Vec2f -> Vec2f -> GameObjectCreation
createMagentaWrapAroundBall pos vel = do
    liftIO $ putStrLn $ "Creating magenta ball at " ++ show pos
    drawComponent <- createCircle 5 magenta
    return (createGameObject drawComponent encloseByWrapAroundB pos vel)

createWhiteNoopBall :: Vec2f -> GameObjectCreation
createWhiteNoopBall pos = do
    liftIO $ putStrLn $ "Creating white noop ball " ++ show pos
    drawComponent <- createCircle 5 white
    return (createStaticGameObject drawComponent pos)

createDeadManWalking :: Vec2f -> GameObjectCreation
createDeadManWalking pos = do
    liftIO $ putStrLn $ "Creating dead man walking noop triangle " ++ show pos
    drawComponent <- createConvex white [Vec2f 55 30, Vec2f 70 60, Vec2f 40 60]
    return (createGameObject drawComponent deadManWalkingB pos zero)

createSimpleHexagon :: Vec2f -> GameObjectCreation
createSimpleHexagon pos = do
    liftIO $ putStrLn $ "Creating a simple hexagon " ++ show pos
    drawComponent <- createHexagon 25.0 white
    return (createGameObject drawComponent (rotateB 1.0) pos zero)

createSimpleEqTriangle :: Vec2f -> GameObjectCreation
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


createMousePositionCopier :: GameObjectCreation
createMousePositionCopier = do
    liftIO $ putStrLn "Creating mouse pointer"
    drawComponent <- createCenteredCircle 3 green
    return (createGameObject drawComponent mousePositionCopierB zero zero)

createMouseFollower :: Vec2f -> GameObjectCreation
createMouseFollower pos = do
    liftIO $ putStrLn "Creating mouse follower"
    drawComponent <- createCenteredCircle 10 blue
    return (createGameObject drawComponent mouseFollowerB pos zero)

createSimpleText :: Vec2f -> String -> GameObjectCreation
createSimpleText pos text = do
    liftIO $ putStrLn "Creating simple text"
    drawComponent <- createText 30 text
    return (createStaticGameObject drawComponent pos)

createLiveGameObjectCounter :: Vec2f -> GameObjectCreation
createLiveGameObjectCounter pos = do
    liftIO $ putStrLn "Creating live GameObject counter"
    drawComponent <- createEmptyText 15
    let behavior = updatePromptForGOCountB "GameObjects"
    return (createStaticGameObjectB drawComponent pos behavior)

createDeathByUpdates :: Vec2f -> GameObjectCreation
createDeathByUpdates pos = do
    liftIO $ putStrLn "Creating object that dies from updates"
    drawComponent <- createCenteredCircle 10 blue
    let behavior = deathByUpdatesB
    return (createStaticGameObjectB drawComponent pos behavior)

createDeathByHitsOnWall :: Vec2f -> Vec2f -> GameObjectCreation
createDeathByHitsOnWall pos vel = do
    liftIO $ putStrLn "Creating object that dies from hitting on walls"
    drawComponent <- createCenteredCircle 15 green
    let behavior = deathByHitsOnWallB
    return (createGameObject drawComponent behavior pos vel)

createMousePositionPrinter :: Vec2f -> GameObjectCreation
createMousePositionPrinter pos = do
    liftIO $ putStrLn "Creating object that prints mouse position"
    drawComponent <- createEmptyText 10
    let behavior = updateTextWithMousePositionB
    return (createGameObject drawComponent behavior pos zero)

createSprite :: FilePath -> Vec2f -> Vec2f -> GameObjectCreation
createSprite path pos vel = do
    liftIO $ putStrLn ("Creating a Sprite GameObject from " ++ path)
    drawComponent <- createSpriteDrawing path
    let behavior = encloseByWrapAroundB
    return (createGameObject drawComponent behavior pos vel)

createMultiplier :: Vec2f -> GameObjectCreation
createMultiplier pos = do
    liftIO $ putStrLn "Creating object that creates children"
    drawComponent <- createCenteredCircle 5 green
    let behavior = noopB
    let childrenFactory = (`createSimpleText` "this is a text")
    let firstChild = childrenFactory (Vec2f 500 40)
    let firstChild' = childrenFactory (Vec2f 500 80)
    let firstChild'' = childrenFactory (Vec2f 500 120)
    return (createGameObjectWithChildren drawComponent behavior pos zero [firstChild, firstChild', firstChild''])

createBehaveOnce :: Vec2f -> GameObjectCreation
createBehaveOnce pos = do
    liftIO $ putStrLn "Creating object that behaves once"
    drawComponent <- createCenteredCircle 5 white
    let behavior = behaveOnceB (addChildB $ createWhiteNoopBall (Vec2f 600 230))
    return (createGameObject drawComponent behavior pos zero )