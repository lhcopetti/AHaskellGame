module Command.MessageCommand
    ( sendDrwMsgCommand
    ) where

import System.Messaging.DrawingMessage (addInbox)
import GameObject.GameObjectTypes (CommandType, DrawingMessage)
import GameObject.GameObject ()

sendDrwMsgCommand :: DrawingMessage -> CommandType
sendDrwMsgCommand msg = return . addInbox msg