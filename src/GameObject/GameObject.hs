{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
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
import NativeResource
import qualified Component.Position as Pos
import Component.Draw.ZOrderable
import Component.Draw.ZDrawing
import Component.Behavior.Behavior
import System.Messaging.DrawingInbox
import qualified System.Messaging.PhysicsInbox as PM
import GameObject.GameObjectTypes
import Command.Command (runCommands)
import Physics.DestroyObject (destroyPhysics)

instance Updatable (GameObject st) st where
    update go = do 
        let noDrawingMsgs = clearInbox go
        updatedObj <- runInput (inputComp go) noDrawingMsgs
        updatedObj' <- behave (behavior updatedObj) updatedObj
        updatedObj'' <- runCommands updatedObj'
        updateDrawing updatedObj''

updateDrawing :: GoUpdateType st
updateDrawing obj@GameObject{ drawComp } = do
    newDrawing <- update drawComp
    return $ obj { drawComp = newDrawing }

instance Synchronizable (GameObject a) where
    synchronize go = syncZDrawing (drawComp go) go

instance Drawable (GameObject a) where 
    draw wnd GameObject { drawComp } = draw wnd drawComp

instance Pos.Position (GameObject a) where
    getPosition = position
    setPosition go newPosition = go { position = newPosition } 
    getRotation = rotation
    setRotation newRotation go = go { rotation = newRotation }

instance Killable (GameObject a) where 
    isAlive = alive
    die g = g { alive = False }

instance NativeResource (GameObject a) where
    free GameObject { drawComp, physicsComp } =
        free drawComp >> destroyPhysics physicsComp

instance DrawingInbox (GameObject a) where
    getInbox = inbox
    addInbox msg obj@GameObject { inbox } = obj { inbox = msg : inbox }
    clearInbox g = g { inbox = [] }

instance PM.PhysicsInbox (GameObject a) where
    getInbox = physicsInbox
    addInbox msg obj@GameObject { physicsInbox } = obj { physicsInbox = msg : physicsInbox }
    clearInbox g = g { physicsInbox = [] }

instance Behavioral (GameObject st) st where
    setBehavior behav g = g { behavior = behav }
    setBehaviorT behav g = g { behavior = Behavior behav }

instance ChildBearer (GameObject st) st where
    getChildren = childObjects
    removeChildren obj = obj { childObjects = [] }
    addChild  child obj@GameObject { childObjects } = obj { childObjects = CGOC child : childObjects }
    addChildP child obj@GameObject { childObjects } = obj { childObjects = PGOC child : childObjects }

instance ZOrderable (GameObject a) where
    getZ GameObject { drawComp } = getZ drawComp
    setZ z obj@GameObject { drawComp } = obj { drawComp = setZ z drawComp }

addCommand :: Command st -> GameObject st -> GameObject st
addCommand comm obj@GameObject { commands } = obj { commands = comm : commands }

addCommandM :: Command st -> CommandType st
addCommandM comm obj = return (addCommand comm obj)