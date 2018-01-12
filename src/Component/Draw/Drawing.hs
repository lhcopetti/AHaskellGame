module Component.Draw.Drawing
    ( Drawing (..)
    , updateDrawing
    , destroyDrawing
    ) where

import SFML.Graphics.Types (CircleShape, RectangleShape, ConvexShape)
import SFML.Graphics.RenderWindow (drawCircle, drawRectangle, drawConvexShape)
import SFML.Graphics.CircleShape
import SFML.Graphics.RectangleShape
import SFML.Graphics.ConvexShape
import SFML.SFResource

import qualified Component.Position as Pos
import Drawable

data Drawing = CircleDrawing CircleShape
             | RectangleDrawing RectangleShape
             | ConvexDrawing ConvexShape

instance Drawable Drawing where
    draw wnd (CircleDrawing ptr) = drawCircle wnd ptr Nothing
    draw wnd (RectangleDrawing ptr) = drawRectangle wnd ptr Nothing
    draw wnd (ConvexDrawing ptr) = drawConvexShape wnd ptr Nothing


updateDrawing :: Pos.Position a => Drawing -> a -> IO ()
updateDrawing (CircleDrawing shape) obj = setPosition shape (Pos.getPosition obj)
updateDrawing (RectangleDrawing shape) obj = setPosition shape (Pos.getPosition obj)
updateDrawing (ConvexDrawing shape) obj = setPosition shape (Pos.getPosition obj)


destroyDrawing :: Drawing -> IO ()
destroyDrawing (CircleDrawing ptr) = destroy ptr
destroyDrawing (RectangleDrawing ptr) = destroy ptr
destroyDrawing (ConvexDrawing ptr) = destroy ptr