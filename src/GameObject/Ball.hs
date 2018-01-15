{-# LANGUAGE NamedFieldPuns #-}
module GameObject.Ball 
    ( Ball (..)
    , draw
    , update
    , synchronize
    ) where
    
import SFML.System.Vector2
import SFML.Graphics.Types
import Control.Monad.Trans.Maybe (MaybeT)

import Updatable
import Synchronizable
import Drawable
import Killable
import qualified Component.Position as Pos
import Component.Physics.PhysicsClass
import Component.Physics.Physics
import Component.Draw.Drawing
import Component.Behavior.Behavior

data Ball = Ball { drawComp     :: Drawing
                 , behavior     :: Behavior
                 , physicsComp  :: Physics
                 , position     :: Vec2f
                 , alive        :: Bool
                 }

instance Updatable Ball where
    update ball@Ball { behavior } = behave behavior ball

instance Synchronizable Ball where
    synchronize ball = updateDrawing (drawComp ball) ball

instance Drawable Ball where 
    draw wnd Ball { drawComp } = draw wnd drawComp

instance Pos.Position Ball where
    getPosition = position
    setPosition ball newPosition = ball { position = newPosition } 

instance PhysicsClass Ball where
    getVelocity = velocity . physicsComp
    setVelocity ball@Ball { physicsComp } newVel = ball { physicsComp = setVelocity physicsComp newVel }

instance Killable Ball where 
    isAlive = alive
    die b = b { alive = False }
    destroyResource Ball { drawComp } = destroyDrawing drawComp