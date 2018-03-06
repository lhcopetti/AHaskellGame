module System.Messaging.Messages.TextDrawingMessage
    ( setTextMsg
    , setTextColorMsg
    ) where

import SFML.Graphics.Text (setTextString, setTextColor)
import SFML.Graphics.Color (Color)

import GameObject.GameObjectTypes (DrawingMessageType, Drawing (..))

setTextMsg :: String -> DrawingMessageType
setTextMsg text (TextDrawing ptr) = setTextString ptr text
setTextMsg _ _ = return ()

setTextColorMsg :: Color -> DrawingMessageType
setTextColorMsg color (TextDrawing ptr) = setTextColor ptr color
setTextColorMsg _ _ = return ()