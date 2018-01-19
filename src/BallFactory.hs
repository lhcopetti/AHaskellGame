module BallFactory
    ( createBall
    , createRedBall
    , createYellowSquare
    , createCyanTriangle
    , createMagentaWrapAroundBall
    , createWhiteNoopBall
    , createDeadManWalking
    , createMousePositionCopier
    , createMouseFollower
    , createMiniBall
    , createSimpleHexagon
    , createSimpleEqTriangle
    , createSimpleText
    , createLiveGameObjectCounter
    ) where

import SFML.System.Vector2
import SFML.Graphics.Color

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createGameObject, createStaticGameObject)
import GameObject.Ball (Ball)
import Component.Draw.CircleDrawing (createCircle, createCenteredCircle)
import Component.Draw.RectangleDrawing (createSquare)
import Component.Draw.ConvexDrawing (createConvex)
import Component.Draw.HexagonDrawing (createHexagon)
import Component.Draw.TriangleDrawing (createEqTriangle)
import Component.Draw.TextDrawing (createText)
import Component.Behavior.Behaviors
import Vec2.Vec2Math (zero)

createMiniBall :: Vec2f -> Vec2f -> MaybeT IO Ball
createMiniBall pos vel = do
    liftIO $ putStrLn $ "Creating mini ball at " ++ show pos
    let color = blue
    shape <- createCircle 2 color
    return (createGameObject shape encloseToBoxB pos vel)    

createBall :: Vec2f -> Vec2f -> MaybeT IO Ball
createBall pos@(Vec2f x y) vel = do 
    liftIO $ putStrLn $ "Creating ball at " ++ show pos
    let color = blue
    shape <- createCircle 25 color
    return (createGameObject shape encloseToBoxB pos vel)

createRedBall :: Vec2f -> Vec2f -> MaybeT IO Ball
createRedBall pos vel = do 
    liftIO $ putStrLn $ "Creating red ball at " ++ show pos
    drawComponent <- createCircle 10 red
    return (createGameObject drawComponent encloseToBoxB pos vel)

createYellowSquare :: Vec2f -> Vec2f -> MaybeT IO Ball
createYellowSquare pos vel = do
    liftIO $ putStrLn $ "Creating yellow square at " ++ show pos
    drawComponent <- createSquare 5 yellow
    return (createGameObject drawComponent encloseByWrapAroundB pos vel)

createCyanTriangle :: Vec2f -> Vec2f -> MaybeT IO Ball
createCyanTriangle pos vel = do
    liftIO $ putStrLn $ "Creating blue triangle at " ++ show pos
    drawComponent <- createConvex cyan [Vec2f 55 30, Vec2f 70 60, Vec2f 40 60]
    return (createGameObject drawComponent encloseToBoxB pos vel)

createMagentaWrapAroundBall :: Vec2f -> Vec2f -> MaybeT IO Ball
createMagentaWrapAroundBall pos vel = do
    liftIO $ putStrLn $ "Creating magenta ball at " ++ show pos
    drawComponent <- createCircle 5 magenta
    return (createGameObject drawComponent encloseByWrapAroundB pos vel)

createWhiteNoopBall :: Vec2f -> MaybeT IO Ball
createWhiteNoopBall pos = do
    liftIO $ putStrLn $ "Creating white noop ball " ++ show pos
    drawComponent <- createCircle 5 white
    return (createStaticGameObject drawComponent pos)

createDeadManWalking :: Vec2f -> MaybeT IO Ball
createDeadManWalking pos = do
    liftIO $ putStrLn $ "Creating dead man walking noop triangle " ++ show pos
    drawComponent <- createConvex white [Vec2f 55 30, Vec2f 70 60, Vec2f 40 60]
    return (createGameObject drawComponent deadManWalkingB pos zero)

createSimpleHexagon :: Vec2f -> MaybeT IO Ball
createSimpleHexagon pos = do
    liftIO $ putStrLn $ "Creating a simple hexagon " ++ show pos
    drawComponent <- createHexagon 25.0 white
    return (createGameObject drawComponent (rotateB 1.0) pos zero)

createSimpleEqTriangle :: Vec2f -> MaybeT IO Ball
createSimpleEqTriangle pos = do
    liftIO $ putStrLn $ "Creating a simple hexagon " ++ show pos
    drawComponent <- createEqTriangle 25.0 white
    return (createGameObject drawComponent followPointingMouseB pos zero)

createMousePositionCopier :: MaybeT IO Ball
createMousePositionCopier = do
    liftIO $ putStrLn "Creating mouse pointer"
    drawComponent <- createCenteredCircle 3 green
    return (createGameObject drawComponent mousePositionCopierB zero zero)

createMouseFollower :: Vec2f -> MaybeT IO Ball
createMouseFollower pos = do
    liftIO $ putStrLn "Creating mouse follower"
    drawComponent <- createCenteredCircle 10 blue
    return (createGameObject drawComponent mouseFollowerB pos zero)

createSimpleText :: Vec2f -> String -> MaybeT IO Ball
createSimpleText pos text = do
    liftIO $ putStrLn "Creating simple text"
    drawComponent <- createText 30 text
    return (createStaticGameObject drawComponent pos)

createLiveGameObjectCounter :: Vec2f -> MaybeT IO Ball
createLiveGameObjectCounter pos = do
    liftIO $ putStrLn "Creating live GameObject counter"
    drawComponent <- createText 15 "Current count of GOs: "
    return (createStaticGameObject drawComponent pos)