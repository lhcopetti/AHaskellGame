module Component.Draw.Drawing
    ( Drawing (..)
    , setOriginDrawing
    , updateDrawing
    , destroyDrawing
    ) where

import SFML.Graphics.Types (CircleShape, RectangleShape, ConvexShape, Text)
import SFML.Graphics.RenderWindow (drawCircle, drawRectangle, drawConvexShape, drawText, drawSprite)
import SFML.Graphics.CircleShape
import SFML.Graphics.RectangleShape
import SFML.Graphics.ConvexShape
import SFML.Graphics.Text
import SFML.Graphics.Sprite
import SFML.Graphics.Texture
import SFML.SFResource
import SFML.System.Vector2 (Vec2f)

import qualified Component.Position as Pos
import Component.Draw.DrawingData
import Drawable
import System.Messaging.DrawingMessage

instance Drawable Drawing where
    draw wnd (CircleDrawing ptr) = drawCircle wnd ptr Nothing
    draw wnd (RectangleDrawing ptr) = drawRectangle wnd ptr Nothing
    draw wnd (ConvexDrawing ptr) = drawConvexShape wnd ptr Nothing
    draw wnd (TextDrawing ptr) = drawText wnd ptr Nothing
    draw wnd (SpriteDrawing sprite _) = drawSprite wnd sprite Nothing
    draw wnd (CompositeDrawing drws) = mapM_ (draw wnd) drws

setOriginDrawing :: Drawing -> Vec2f -> IO ()
setOriginDrawing (CircleDrawing     ptr) pos = setOrigin ptr pos
setOriginDrawing (RectangleDrawing  ptr) pos = setOrigin ptr pos
setOriginDrawing (ConvexDrawing     ptr) pos = setOrigin ptr pos
setOriginDrawing (TextDrawing       ptr) pos = setOrigin ptr pos
setOriginDrawing (SpriteDrawing   ptr _) pos = setOrigin ptr pos
setOriginDrawing (CompositeDrawing drws) pos = mapM_ (`setOriginDrawing` pos) drws
    

updateDrawing :: (Pos.Position a, DrawingInbox a) => Drawing -> a -> IO ()
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
    executeMessages (TextDrawing text) (getInbox obj)
updateDrawing (SpriteDrawing shape _) obj = do
    setPosition shape (Pos.getPosition obj)
    setRotation shape (Pos.getRotation obj)
updateDrawing (CompositeDrawing drws) obj = mapM_ (`updateDrawing` obj) drws


executeMessages :: Drawing -> [DrawingMessage] -> IO ()
executeMessages drw = mapM_ (executeMessage drw)

executeMessage :: Drawing -> DrawingMessage -> IO ()
executeMessage drw (MSG f) = f drw

destroyDrawing :: Drawing -> IO ()
destroyDrawing (CircleDrawing       ptr ) = destroy ptr
destroyDrawing (RectangleDrawing    ptr ) = destroy ptr
destroyDrawing (ConvexDrawing       ptr ) = destroy ptr
destroyDrawing (TextDrawing         ptr ) = destroy ptr
destroyDrawing (SpriteDrawing   spr tex ) = destroy spr >> destroy tex
destroyDrawing (CompositeDrawing    drws) = mapM_ destroyDrawing drws