module System.Messaging.TextDrawingMessage
    ( setTextMsg
    ) where

import SFML.Graphics.Text (setTextString)

import Component.Draw.DrawingData (Drawing (..))
import System.Messaging.DrawingMessage (DrawingMessageType)

setTextMsg :: String -> DrawingMessageType
setTextMsg text (TextDrawing ptr) = setTextString ptr text
setTextMsg _ _ = return ()