module GameObject.TextGameObject
    ( createTextGO
    )
    where

import SFML.Graphics.Color

import Control.Monad.IO.Class (liftIO)

import Component.Draw.TextDrawing (createText)
import System.Messaging.Handler.RunMessageHandler (runMessageT)
import System.Messaging.Messages.TextDrawingMessage (setTextColorMsg)
import GameObject.GameObjectTypes
import GameObjectFactory
import Vec2.Vec2Math (zero)

createTextGO :: Int -> Color -> String -> GameObjectCreation
createTextGO size color text = do
    txt <- createText size text
    liftIO $ runMessageT (setTextColorMsg color) txt
    return (createStaticGameObject txt zero)