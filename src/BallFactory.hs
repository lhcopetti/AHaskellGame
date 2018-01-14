module BallFactory
    ( createBall
    , createRedBall
    , createYellowSquare
    , createCyanTriangle
    , createMagentaWrapAroundBall
    , createWhiteNoopBall
    ) where

import SFML.System.Vector2
import SFML.Graphics.Color

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createGameObject, createStaticGameObject)
import GameObject.Ball (Ball)
import Component.Draw.CircleDrawing (createCircle)
import Component.Draw.RectangleDrawing (createSquare)
import Component.Draw.ConvexDrawing (createConvex)
import Component.Behavior.Behaviors (encloseToBoxB, encloseByWrapAroundB)

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