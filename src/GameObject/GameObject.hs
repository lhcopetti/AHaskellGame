{-# LANGUAGE NamedFieldPuns #-}
module GameObject.GameObject
    ( GameObject (..)
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
import System.Messaging.DrawingMessage

data GameObject = GameObject { drawComp     :: Drawing
                             , behavior     :: Behavior
                             , physicsComp  :: Physics
                             , position     :: Vec2f
                             , rotation     :: Float
                             , inbox         :: [DrawingMessage]
                             , alive        :: Bool
                             }

instance Updatable GameObject where
    update ball@GameObject { behavior } = do 
        let updatedBall = updatePhysics ball
        behave behavior updatedBall

instance Synchronizable GameObject where
    synchronize ball = updateDrawing (drawComp ball) ball

instance Drawable GameObject where 
    draw wnd GameObject { drawComp } = draw wnd drawComp

instance Pos.Position GameObject where
    getPosition = position
    setPosition ball newPosition = ball { position = newPosition } 
    getRotation = rotation
    setRotation newRotation ball = ball { rotation = newRotation }

instance PhysicsClass GameObject where
    getVelocity = velocity . physicsComp
    setVelocity ball@GameObject { physicsComp } newVel = ball { physicsComp = setVelocity physicsComp newVel }

instance Killable GameObject where 
    isAlive = alive
    die b = b { alive = False }
    destroyResource GameObject { drawComp } = destroyDrawing drawComp

instance DrawingInbox GameObject where
    getInbox = inbox
    setInbox newMsgs b = b { inbox = newMsgs }

instance Behavioral GameObject where
    setBehavior behav b = b { behavior = behav }
    setBehaviorT behav b = b { behavior = Behavior behav }