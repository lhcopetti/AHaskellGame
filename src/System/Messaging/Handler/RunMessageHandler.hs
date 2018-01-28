module System.Messaging.Handler.RunMessageHandler
    ( runMessage
    , runMessages
    ) where

import Control.Monad (when)

import Component.Draw.DrawingData
import System.Messaging.DrawingMessage

runMessages :: [DrawingMessage] -> Drawing -> IO ()
runMessages msgs drw = mapM_ (`runMessage` drw) msgs

runMessage :: DrawingMessage -> Drawing -> IO ()
runMessage  (NamedMessage nameMsg f)
            (NamedDrawing nameDrw drw)   =
            when (nameDrw == nameMsg) (f drw)
runMessage (MSG f)              drw = f drw
runMessage (NamedMessage _ _)   _   = return ()