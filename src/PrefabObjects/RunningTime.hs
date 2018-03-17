module PrefabObjects.RunningTime
    ( mkRunningTime
    ) where

import Control.Monad.Reader (asks)

import Time.Time
import GameEnv
import GameObjectFactory
import GameObject.GameObjectTypes
import Component.Draw.TextDrawing (createEmptyText)
import System.Messaging.Messages.TextDrawingMessage (setTextMsg)
import System.Messaging.Handler.PushMessageHandler (pushMessage)
import Vec2.Vec2Math (zero)

mkRunningTime :: GameObjectCreation st
mkRunningTime = do
    drawing <- createEmptyText 10
    let beh = Behavior updateTextWithRunningTime
    return (createStaticGameObjectB drawing zero beh)

updateTextWithRunningTime :: BehaviorType st
updateTextWithRunningTime obj = do
    time <- asks (runningTime . time)
    let txt = formatSeconds (floor time)
    pushMessage (setTextMsg txt) obj
