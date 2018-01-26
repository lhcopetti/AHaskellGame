module System.Messaging.MessageHelper
    ( pushNamedMessage
    ) where

import GameObject.GameObjectTypes (Command (..), CommandType)
import GameObject.GameObject (addCommand)
import System.Messaging.DrawingMessage (DrawingMessage (..), DrawingMessageType)

import Command.MessageCommand (sendDrwMsgCommand)

pushNamedMessage :: String -> DrawingMessageType -> CommandType
pushNamedMessage msgName msg obj = let
    namedMsg = NamedMessage msgName msg
    commandMsg = Command $ sendDrwMsgCommand namedMsg
    in
        return (addCommand commandMsg obj)