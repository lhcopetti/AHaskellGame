module PrefabObjects.CollisionPointsCounter
    ( mkCollisionPointsCounter
    ) where

import SFML.System.Vector2

import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createSimplePhysicsGO)
import GameObject.GameObjectTypes
import System.Messaging.Handler.PushMessageHandler (pushMessage)
import System.Messaging.Messages.TextDrawingMessage (setTextMsg)
import Component.Draw.Drawing ()
import Component.Draw.TextDrawing (createText)
import Component.Behavior.CollisionBehavior (getCollisionPointCount)
import Vec2.Vec2Math (zero)

mkCollisionPointsCounter :: GameObjectCreation st
mkCollisionPointsCounter = do
    liftIO $ putStrLn "Creating a collision counter"
    text <- createText 15 "Collision count"
    let beh = Behavior countCollisions
    let go = createSimplePhysicsGO text beh (Vec2f 0 0) zero
    return go

countCollisions :: BehaviorType st
countCollisions obj = do
    points <- getCollisionPointCount
    let msg = "Collision points count: " ++ show (length points)
    pushMessage (setTextMsg msg) obj