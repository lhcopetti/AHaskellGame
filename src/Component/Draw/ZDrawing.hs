module Component.Draw.ZDrawing
    ( syncZDrawing
    , mkZDrawing
    ) where

import SFML.Graphics.RenderWindow (drawCircle, drawRectangle, drawConvexShape, drawText, drawSprite)
import SFML.SFResource (destroy)

import Control.Monad (liftM)

import qualified Component.Position as Pos
import Component.Draw.DrawingSync (executeUpdateOnDrawing)
import Component.Draw.Drawing
import Drawable
import Updatable
import NativeResource
import Synchronizable
import System.Messaging.DrawingMessage
import GameObject.GameObjectTypes
import Component.Draw.Animation.AnimationDrawing (updateAnimation)

instance Drawable ZDrawing where
    draw wnd (ZDrawing drw _) = draw wnd drw

instance Updatable ZDrawing where
    update (ZDrawing drw f) = liftM (`ZDrawing` f) (update drw)

syncZDrawing :: (Pos.Position a, DrawingInbox a) => ZDrawing -> a -> IO ()
syncZDrawing (ZDrawing drw _) = syncDrawing drw


instance NativeResource ZDrawing where
    free (ZDrawing drw _)  = free drw

mkZDrawing :: Drawing -> ZDrawing
mkZDrawing drw = ZDrawing drw 0.0