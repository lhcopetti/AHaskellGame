module Command.MessageCommand
    ( sendDrwMsgCommand
    , sendPhyMsgCommand
    ) where

import System.Messaging.DrawingInbox (addInbox)
import qualified System.Messaging.PhysicsInbox as PM (addInbox)
import GameObject.GameObjectTypes (CommandType, DrawingMessage, PhysicsMessage)
import GameObject.GameObject ()

sendDrwMsgCommand :: DrawingMessage -> CommandType st
sendDrwMsgCommand msg = return . addInbox msg

sendPhyMsgCommand :: PhysicsMessage -> CommandType st
sendPhyMsgCommand msg = return . PM.addInbox msg