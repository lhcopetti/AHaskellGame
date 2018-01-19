module Component.Draw.Drawing
    ( Drawing (..)
    , setOriginDrawing
    , updateDrawing
    , destroyDrawing
    ) where

import SFML.Graphics.Types (CircleShape, RectangleShape, ConvexShape, Text)
import SFML.Graphics.RenderWindow (drawCircle, drawRectangle, drawConvexShape, drawText)
import SFML.Graphics.CircleShape
import SFML.Graphics.RectangleShape
import SFML.Graphics.ConvexShape
import SFML.Graphics.Text
import SFML.SFResource
import SFML.System.Vector2 (Vec2f)

import qualified Component.Position as Pos
import Drawable

data Drawing = CircleDrawing CircleShape
             | RectangleDrawing RectangleShape
             | ConvexDrawing ConvexShape
             | TextDrawing Text

instance Drawable Drawing where
    draw wnd (CircleDrawing ptr) = drawCircle wnd ptr Nothing
    draw wnd (RectangleDrawing ptr) = drawRectangle wnd ptr Nothing
    draw wnd (ConvexDrawing ptr) = drawConvexShape wnd ptr Nothing
    draw wnd (TextDrawing ptr) = drawText wnd ptr Nothing

setOriginDrawing :: Drawing -> Vec2f -> IO ()
setOriginDrawing (CircleDrawing     ptr) pos = setOrigin ptr pos
setOriginDrawing (RectangleDrawing  ptr) pos = setOrigin ptr pos
setOriginDrawing (ConvexDrawing     ptr) pos = setOrigin ptr pos
setOriginDrawing (TextDrawing       ptr) pos = setOrigin ptr pos
    

updateDrawing :: Pos.Position a => Drawing -> a -> IO ()
updateDrawing (CircleDrawing shape) obj = do
    setPosition shape (Pos.getPosition obj)
    setRotation shape (Pos.getRotation obj)
updateDrawing (RectangleDrawing shape) obj = do
    setPosition shape (Pos.getPosition obj)
    setRotation shape (Pos.getRotation obj)
updateDrawing (ConvexDrawing shape) obj = do
    setPosition shape (Pos.getPosition obj)
    setRotation shape (Pos.getRotation obj)
updateDrawing (TextDrawing text) obj = do
    setPosition text (Pos.getPosition obj)
    setRotation text (Pos.getRotation obj)


destroyDrawing :: Drawing -> IO ()
destroyDrawing (CircleDrawing ptr) = destroy ptr
destroyDrawing (RectangleDrawing ptr) = destroy ptr
destroyDrawing (ConvexDrawing ptr) = destroy ptr
destroyDrawing (TextDrawing ptr) = destroy ptr