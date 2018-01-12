{-# LANGUAGE NamedFieldPuns #-}
module GameObject.Ball 
    ( Ball (..)
    , createBall
    , createRedBall
    , createYellowSquare
    , createCyanTriangle
    , draw
    , update
    , synchronize
    ) where
    
import SFML.Graphics.Color
import SFML.Graphics.CircleShape
import SFML.System.Vector2
import Vec2.Vec2Math (zero, addVec2f)
import SFML.Graphics.Types
import SFML.Graphics.RenderWindow (drawCircle)
import SFML.Graphics.SFShape (setFillColor)
import SFML.Graphics.SFTransformable (setPosition)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Control.Monad (mzero)

import GameEnv
import Updatable
import Synchronizable
import Drawable
import Killable
import qualified Component.Position as Pos
import qualified Component.Physics as Phy
import Component.Draw.Drawing
import Behavior.BoxedBehavior (boundToDimension)
import Component.Draw.CircleDrawing (createCircle)
import Component.Draw.RectangleDrawing (createSquare)
import Component.Draw.ConvexDrawing (createConvex)

data Ball = Ball { drawComp :: Drawing
                 , position :: Vec2f
                 , velocity :: Vec2f
                 , alive    :: Bool
                 }

instance Updatable Ball where
    update b@Ball { position, velocity } = do
        -- Physics update
        let newB = Pos.setPosition b (addVec2f position velocity)

        -- Behavior update
        dimension <- asks gameArea
        let newBall = boundToDimension newB dimension

        -- Return the new object
        return newBall

instance Synchronizable Ball where
    synchronize ball = updateDrawing (drawComp ball) ball

instance Drawable Ball where 
    draw wnd Ball { drawComp } = draw wnd drawComp

instance Pos.Position Ball where
    getPosition = position
    setPosition ball newPosition = ball { position = newPosition } 

instance Phy.Physics Ball where
    getVelocity = velocity
    setVelocity ball newVel = ball { velocity = newVel }

instance Killable Ball where 
    isAlive = alive
    die b = b { alive = False }
    destroyResource Ball { drawComp } = destroyDrawing drawComp 

createBall :: Vec2f -> Vec2f -> MaybeT IO Ball
createBall pos@(Vec2f x y) vel = do 
    liftIO $ putStrLn $ "Creating ball at " ++ show pos
    let color = blue
    shape <- createCircle 25 color
    return (Ball shape pos vel True)

createRedBall :: Vec2f -> Vec2f -> MaybeT IO Ball
createRedBall pos vel = do 
    liftIO $ putStrLn $ "Creating red ball at " ++ show pos
    drawComponent <- createCircle 10 red
    return (Ball drawComponent pos vel True)

createYellowSquare :: Vec2f -> Vec2f -> MaybeT IO Ball
createYellowSquare pos vel = do
    liftIO $ putStrLn $ "Creating yellow square at " ++ show pos
    drawComponent <- createSquare 5 yellow
    return (Ball drawComponent pos vel True)

createCyanTriangle :: Vec2f -> Vec2f -> MaybeT IO Ball
createCyanTriangle pos vel = do
    liftIO $ putStrLn $ "Creating blue triangle at " ++ show pos
    drawComponent <- createConvex cyan [Vec2f 55 30, Vec2f 70 60, Vec2f 40 60]
    return (Ball drawComponent pos vel True)