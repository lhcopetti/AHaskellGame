module PrefabObjects.MousePositionPrinter
    ( mkMousePrinter
    , mkMousePrinterB
    ) where

import SFML.Graphics.Color

import GameObjectFactory
import GameObject.GameObjectTypes
import Component.Behavior.Behaviors (updateTextWithMousePositionB)
import Component.Behavior.Behavior (setBehaviorT)
import Component.Draw.TextDrawing (createEmptyText)
import Component.Input.MousePosition
import System.Messaging.Messages.TextDrawingMessage (setTextColorMsg)
import System.Messaging.Handler.PushMessageHandler (pushMessage)
import Vec2.Vec2Math (zero)

mkMousePrinter :: GameObjectCreation
mkMousePrinter = do
    drawing <- createEmptyText 10
    let beh = updateTextWithMousePositionB
    return (createSimplePhysicsGO drawing beh zero zero)

transitionTo :: MousePositionState -> DrawingMessageType
transitionTo WithinBounds = setTextColorMsg white
transitionTo OutOfBounds  = setTextColorMsg red

mkMousePrinterB :: GameObjectCreation
mkMousePrinterB = do
    drawing <- createEmptyText 10
    let beh = Behavior (mouseBehavior WithinBounds)
    return (createSimplePhysicsGO drawing beh zero zero)

mouseBehavior :: MousePositionState -> BehaviorType
mouseBehavior currState obj = do
    updated <- behave updateTextWithMousePositionB obj
    newState <- getMouseState
    if newState /= currState then 
        do  newObj <- pushMessage (transitionTo newState) updated
            return $ setBehaviorT (mouseBehavior newState) newObj
    else
        return updated