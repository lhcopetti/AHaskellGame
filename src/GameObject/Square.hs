{-# LANGUAGE NamedFieldPuns #-}
module GameObject.Square
    ( Square(..)
    , createSquare
    , update
    , draw
    ) where

import SFML.Graphics.Color
import SFML.System.Vector2
import SFML.Graphics.RectangleShape
import SFML.Graphics.Types (RectangleShape, RenderWindow)
import SFML.Graphics.RenderWindow

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)

import Vec2.Vec2Math (zero, addVec2f)
import Updatable
import Synchronizable
import Drawable
import Killable
import GameEnv
import Behavior.BoxedBehavior (wrapAround)
import qualified Component.Position as Pos
import qualified Component.Physics  as Phy
import Component.Draw.Drawing
import Component.Draw.RectangleDrawing (createRectangle)

data Square = Square { drawComp  :: Drawing
                     , position :: Vec2f
                     , velocity :: Vec2f
                     , color    :: Color
                     , alive    :: Bool
                     }

instance Updatable Square where
    update s@Square { position, velocity } = do
        let newS = Pos.setPosition s (addVec2f position velocity)
        
        dimension <- asks gameArea
        let newSquare = wrapAround newS dimension

        return newSquare

instance Synchronizable Square where
    synchronize square = updateDrawing (drawComp square) square

instance Drawable Square where 
    draw wnd Square { drawComp } = draw wnd drawComp

instance Pos.Position Square where
    getPosition = position
    setPosition square newPos = square { position = newPos }

instance Phy.Physics Square where
    getVelocity = velocity
    setVelocity square newVel = square { velocity = newVel }

instance Killable Square where
    isAlive = alive
    die s = s { alive = False }
    destroyResource Square { drawComp } = destroyDrawing drawComp 

createSquare :: Vec2f -> Vec2f -> MaybeT IO Square
createSquare pos@(Vec2f x y) vel = do 
    liftIO $ putStrLn $ "Creating square at " ++ show pos
    let color = green
    square <- createRectangle (Vec2f 25 25) color
    return (Square square pos vel color True)