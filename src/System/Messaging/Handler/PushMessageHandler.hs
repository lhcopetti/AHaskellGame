module System.Messaging.Handler.PushMessageHandler
    ( pushMessage
    , pushNamedMessage
    ) where

import GameObject.GameObjectTypes (Command (..), CommandType, DrawingMessage (..), DrawingMessageType)
import GameObject.GameObject (addCommand)

import Command.MessageCommand (sendDrwMsgCommand)

pushMessage :: DrawingMessageType -> CommandType
pushMessage msg = pushDrawingMessage (MSG msg)

pushNamedMessage :: String -> DrawingMessageType -> CommandType
pushNamedMessage msgName msg = pushDrawingMessage (NamedMessage msgName msg)

pushDrawingMessage :: DrawingMessage -> CommandType
pushDrawingMessage msg obj = return (addCommand comm obj)
    where
        comm = Command (sendDrwMsgCommand msg)