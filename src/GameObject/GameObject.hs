{-# LANGUAGE NamedFieldPuns #-}
module GameObject.GameObject
    ( GameObject (..)
    , draw
    , update
    , synchronize
    , addCommand
    , addCommandM
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
        updatedObj <- runInput (inputComp go) noDrawingMsgs
        updatedObj' <- behave (behavior updatedObj) updatedObj
        updatedObj'' <- runCommands updatedObj'
        return (updateDrawing updatedObj'')


instance Synchronizable GameObject where
    synchronize go = syncDrawing (drawComp go) go

instance Drawable GameObject where 
    draw wnd GameObject { drawComp } = draw wnd drawComp

instance Pos.Position GameObject where
    getPosition = position
    setPosition go newPosition = go { position = newPosition } 
    getRotation = rotation
    setRotation newRotation go = go { rotation = newRotation }

instance PhysicsClass GameObject where
    getVelocity = getVelocity . physicsComp
    setVelocity go@GameObject { physicsComp } newVel = go { physicsComp = setVelocity physicsComp newVel }
    updatePhysics = updatePhysicsComponent

instance Killable GameObject where 
    isAlive = alive
    die g = g { alive = False }
    destroyResource GameObject { drawComp } = destroyDrawing drawComp

instance DrawingInbox GameObject where
    getInbox = inbox
    addInbox msg obj@GameObject { inbox } = obj { inbox = msg : inbox }
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

addCommandM :: Command -> CommandType
addCommandM comm obj = return (addCommand comm obj)

updatePhysicsComponent :: GameObject -> IO GameObject
updatePhysicsComponent go = case physicsComp go of
    (SimplePhy _ _) -> return . updatePosition $ go
    _ -> return go