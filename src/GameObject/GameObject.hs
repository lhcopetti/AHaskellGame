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
import GameObject.GameObjectTypes

instance Updatable GameObject where
    update go@GameObject { behavior } = do 
        let updatedBall = updatePhysics go
        behave behavior updatedBall

instance Synchronizable GameObject where
    synchronize go = updateDrawing (drawComp go) go

instance Drawable GameObject where 
    draw wnd GameObject { drawComp } = draw wnd drawComp

instance Pos.Position GameObject where
    getPosition = position
    setPosition go newPosition = go { position = newPosition } 
    getRotation = rotation
    setRotation newRotation go = go { rotation = newRotation }

instance PhysicsClass GameObject where
    getVelocity = velocity . physicsComp
    setVelocity go@GameObject { physicsComp } newVel = go { physicsComp = setVelocity physicsComp newVel }

instance Killable GameObject where 
    isAlive = alive
    die g = g { alive = False }
    destroyResource GameObject { drawComp } = destroyDrawing drawComp

instance DrawingInbox GameObject where
    getInbox = inbox
    setInbox newMsgs g = g { inbox = newMsgs }

instance Behavioral GameObject where
    setBehavior behav g = g { behavior = behav }
    setBehaviorT behav g = g { behavior = Behavior behav }