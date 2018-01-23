module Component.Draw.Drawing
    ( Drawing (..)
    , setOriginDrawing
    , updateDrawing
    , destroyDrawing
    ) where

import Control.Monad (when)

import SFML.Graphics.RenderWindow (drawCircle, drawRectangle, drawConvexShape, drawText, drawSprite)
import SFML.Graphics.CircleShape ()
import SFML.Graphics.RectangleShape ()
import SFML.Graphics.ConvexShape ()
import SFML.Graphics.Text ()
import SFML.Graphics.Sprite ()
import SFML.Graphics.Texture ()
import SFML.Graphics.SFTransformable
import SFML.SFResource (destroy)
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
    draw wnd (FlaggedDrawing drawing _) = draw wnd drawing
    draw wnd (CompositeDrawing drws) = mapM_ (draw wnd) drws

setOriginDrawing :: Drawing -> Vec2f -> IO ()
setOriginDrawing (CircleDrawing     ptr) pos = setOrigin ptr pos
setOriginDrawing (RectangleDrawing  ptr) pos = setOrigin ptr pos
setOriginDrawing (ConvexDrawing     ptr) pos = setOrigin ptr pos
setOriginDrawing (TextDrawing       ptr) pos = setOrigin ptr pos
setOriginDrawing (SpriteDrawing   ptr _) pos = setOrigin ptr pos
setOriginDrawing (FlaggedDrawing  ptr _) pos = setOriginDrawing ptr pos
setOriginDrawing (CompositeDrawing drws) pos = mapM_ (`setOriginDrawing` pos) drws


updateDrawing :: (Pos.Position a, DrawingInbox a) => Drawing -> a -> IO ()
updateDrawing (FlaggedDrawing drw flg) obj = updateDrawingTransformable drw obj (updatePosition, updateRotation)
    where
        updatePosition = NoPositionUpdates `notElem` flg
        updateRotation = NoRotationUpdates `notElem` flg
updateDrawing (CompositeDrawing drws)  obj  = mapM_ (`updateDrawing` obj) drws
updateDrawing drw obj                      = updateAllTransformable drw obj

updateDrawingTransformable :: (Pos.Position a, DrawingInbox a) => Drawing -> a -> (Bool, Bool) -> IO ()
updateDrawingTransformable (CircleDrawing shape)    obj tuple = updateTransformable shape obj tuple
updateDrawingTransformable (RectangleDrawing shape) obj tuple = updateTransformable shape obj tuple
updateDrawingTransformable (ConvexDrawing shape)    obj tuple = updateTransformable shape obj tuple
updateDrawingTransformable (SpriteDrawing shape _)  obj tuple = updateTransformable shape obj tuple
updateDrawingTransformable (CompositeDrawing drws)  obj tuple = mapM_ (updateDrawingTransformableFlip obj tuple) drws
updateDrawingTransformable (TextDrawing text)       obj tuple = do
    updateTransformable text obj tuple
    executeMessages (TextDrawing text) (getInbox obj)

updateDrawingTransformableFlip :: (Pos.Position a, DrawingInbox a) => a -> (Bool, Bool) -> Drawing -> IO ()
updateDrawingTransformableFlip obj (pos, rot) drw = updateDrawingTransformable drw obj (pos, rot)

updateAllTransformable :: (Pos.Position a, DrawingInbox a) => Drawing -> a -> IO ()
updateAllTransformable drw obj = updateDrawingTransformable drw obj (True, True)

updateTransformable :: (SFTransformable a, Pos.Position b) => a -> b -> (Bool, Bool) -> IO ()
updateTransformable ptr obj (pos, rot) = do
    when pos (setPosition ptr (Pos.getPosition obj))
    when rot (setRotation ptr (Pos.getRotation obj))

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
destroyDrawing (FlaggedDrawing    ptr _ ) = destroyDrawing ptr