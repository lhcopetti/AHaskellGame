{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Component.Draw.ZDrawing
    ( syncZDrawing
    , mkZDrawing
    ) where

import Control.Monad (liftM)

import qualified Component.Position as Pos
import Component.Draw.Drawing
import Component.Draw.ZOrderable
import Drawable
import Updatable
import NativeResource
import System.Messaging.DrawingMessage
import GameObject.GameObjectTypes

instance Drawable ZDrawing where
    draw wnd (ZDrawing drw _) = draw wnd drw

instance Updatable ZDrawing st where
    update (ZDrawing drw f) = liftM (`ZDrawing` f) (update drw)

syncZDrawing :: (Pos.Position a, DrawingInbox a) => ZDrawing -> a -> IO ()
syncZDrawing (ZDrawing drw _) = syncDrawing drw


instance NativeResource ZDrawing where
    free (ZDrawing drw _)  = free drw

mkZDrawing :: Drawing -> ZDrawing
mkZDrawing drw = ZDrawing drw 0.0

instance ZOrderable ZDrawing where
    getZ (ZDrawing _ z) = z
    setZ z (ZDrawing drw _) = ZDrawing drw z