{-# LANGUAGE NamedFieldPuns #-}
module GameObject.GameObject
    ( GameObject (..)
    , draw
    , update
    , synchronize
    , addCommand
    ) where

import Updatable
import Synchronizable
import Drawable
import Killable
import ChildBearer
import qualified Component.Position as Pos
import Component.Physics.PhysicsClass
import Component.Physics.Physics
import Component.Draw.Drawing
import Component.Behavior.Behavior
import System.Messaging.DrawingMessage
import GameObject.GameObjectTypes
import Command.Command (runCommands)

instance Updatable GameObject where
    update go = do 
        let noDrawingMsgs = clearInbox go
        let updatedPhysics = updatePhysics noDrawingMsgs
        updatedObj <- runInput (inputComp go) updatedPhysics
        updatedObj' <- behave (behavior updatedObj) updatedObj
        runCommands updatedObj'


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
    clearInbox g = g { inbox = [] }

instance Behavioral GameObject where
    setBehavior behav g = g { behavior = behav }
    setBehaviorT behav g = g { behavior = Behavior behav }

instance ChildBearer GameObject where
    getChildren = childObjects
    removeChildren obj = obj { childObjects = [] }
    addChild child obj@GameObject { childObjects } = obj { childObjects = child : childObjects }

addCommand :: Command -> GameObject -> GameObject
addCommand comm obj@GameObject { commands } = obj { commands = comm : commands }