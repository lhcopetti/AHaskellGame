module System.Messaging.Handler.RunMessageHandler
    ( runMessage
    , runMessages
    ) where

import Control.Monad (when)

import Component.Draw.DrawingData
import System.Messaging.DrawingMessage

runMessages :: Drawing -> [DrawingMessage] -> IO ()
runMessages drw = mapM_ (runMessage drw)

runMessage :: Drawing -> DrawingMessage -> IO ()
runMessage  (NamedDrawing nameDrw drw)   
            (NamedMessage nameMsg f)   =
            when (nameDrw == nameMsg) (f drw)
runMessage drw (MSG f) = f drw
runMessage _ (NamedMessage _ _) = return ()