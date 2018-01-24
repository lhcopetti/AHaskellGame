module Command.MessageCommand
    ( sendDrwMsgCommand
    ) where

import System.Messaging.DrawingMessage (DrawingMessage, setInbox)
import GameObject.GameObjectTypes (CommandType)
import GameObject.GameObject ()

sendDrwMsgCommand :: DrawingMessage -> CommandType
sendDrwMsgCommand msg = setInbox [msg]