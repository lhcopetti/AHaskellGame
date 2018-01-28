module System.Messaging.Handler.RunMessageHandler
    ( runMessage
    , runMessages
    , runMessageT
    , runNamedMessageT
    ) where

import Control.Monad (when)

import GameObject.GameObjectTypes (DrawingMessage, DrawingMessageType, DrawingMessage (..), Drawing (NamedDrawing))

runMessages :: [DrawingMessage] -> DrawingMessageType
runMessages msgs drw = mapM_ (`runMessage` drw) msgs

runMessage :: DrawingMessage -> DrawingMessageType
runMessage  (NamedMessage nameMsg f)
            (NamedDrawing nameDrw drw)   =
            when (nameDrw == nameMsg) (f drw)
runMessage (MSG f)              drw = f drw
runMessage (NamedMessage _ _)   _   = return ()

runMessageT :: DrawingMessageType -> DrawingMessageType
runMessageT msgType = runMessage (MSG msgType)

runNamedMessageT :: String -> DrawingMessageType -> DrawingMessageType
runNamedMessageT msgName msgType = runMessage (NamedMessage msgName msgType)