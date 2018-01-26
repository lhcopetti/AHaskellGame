module Command.MessageCommand
    ( sendDrwMsgCommand
    ) where

import System.Messaging.DrawingMessage (DrawingMessage, addInbox)
import GameObject.GameObjectTypes (CommandType)
import GameObject.GameObject ()

sendDrwMsgCommand :: DrawingMessage -> CommandType
sendDrwMsgCommand msg = return . addInbox msg