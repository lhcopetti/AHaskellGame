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

import GameObject.Ball
import Component.Draw.CircleDrawing (createCircle)
import Component.Draw.RectangleDrawing (createSquare)
import Component.Draw.ConvexDrawing (createConvex)
import Component.Behavior.Behaviors (encloseToBoxB, encloseByWrapAroundB, noopB)

createBall :: Vec2f -> Vec2f -> MaybeT IO Ball
createBall pos@(Vec2f x y) vel = do 
    liftIO $ putStrLn $ "Creating ball at " ++ show pos
    let color = blue
    shape <- createCircle 25 color
    return (Ball shape encloseToBoxB pos vel True)

createRedBall :: Vec2f -> Vec2f -> MaybeT IO Ball
createRedBall pos vel = do 
    liftIO $ putStrLn $ "Creating red ball at " ++ show pos
    drawComponent <- createCircle 10 red
    return (Ball drawComponent encloseToBoxB pos vel True)

createYellowSquare :: Vec2f -> Vec2f -> MaybeT IO Ball
createYellowSquare pos vel = do
    liftIO $ putStrLn $ "Creating yellow square at " ++ show pos
    drawComponent <- createSquare 5 yellow
    return (Ball drawComponent encloseByWrapAroundB pos vel True)

createCyanTriangle :: Vec2f -> Vec2f -> MaybeT IO Ball
createCyanTriangle pos vel = do
    liftIO $ putStrLn $ "Creating blue triangle at " ++ show pos
    drawComponent <- createConvex cyan [Vec2f 55 30, Vec2f 70 60, Vec2f 40 60]
    return (Ball drawComponent encloseToBoxB pos vel True)

createMagentaWrapAroundBall :: Vec2f -> Vec2f -> MaybeT IO Ball
createMagentaWrapAroundBall pos vel = do
    liftIO $ putStrLn $ "Creating magenta ball at " ++ show pos
    drawComponent <- createCircle 5 magenta
    return (Ball drawComponent encloseByWrapAroundB pos vel True)

createWhiteNoopBall :: Vec2f -> Vec2f -> MaybeT IO Ball
createWhiteNoopBall pos vel = do
    liftIO $ putStrLn $ "Creating white noop ball " ++ show pos
    drawComponent <- createCircle 5 white
    return (Ball drawComponent noopB pos vel True)