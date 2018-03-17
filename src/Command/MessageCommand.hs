module Command.MessageCommand
    ( sendDrwMsgCommand
    ) where

import System.Messaging.DrawingMessage (addInbox)
import GameObject.GameObjectTypes (CommandType, DrawingMessage)
import GameObject.GameObject ()

sendDrwMsgCommand :: DrawingMessage -> CommandType st
sendDrwMsgCommand msg = return . addInbox msg