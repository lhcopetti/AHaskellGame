module System.Messaging.Messages.TextDrawingMessage
    ( setTextMsg
    ) where

import SFML.Graphics.Text (setTextString)

import GameObject.GameObjectTypes (DrawingMessageType, Drawing (..))

setTextMsg :: String -> DrawingMessageType
setTextMsg text (TextDrawing ptr) = setTextString ptr text
setTextMsg _ _ = return ()