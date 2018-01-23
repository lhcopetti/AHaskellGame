module Component.Draw.DrawingUpdate
    ( updateDrawingTransformable
    , updateAllTransformable
    ) where

import Control.Monad (when, forM_)

import SFML.Graphics.CircleShape ()
import SFML.Graphics.RectangleShape ()
import SFML.Graphics.ConvexShape ()
import SFML.Graphics.Text ()
import SFML.Graphics.Sprite ()
import SFML.Graphics.Texture ()
import SFML.Graphics.SFTransformable

import qualified Component.Position as Pos
import Component.Draw.DrawingData
import System.Messaging.DrawingMessage

updateDrawingTransformable :: (Pos.Position a, DrawingInbox a) => Drawing -> a -> (Bool, Bool) -> IO ()
updateDrawingTransformable (CircleDrawing shape)    obj tuple = do
    updateTransformable shape obj tuple
    executeMessages (CircleDrawing shape) (getInbox obj)
updateDrawingTransformable d@(RectangleDrawing shape) obj tuple = do
    updateTransformable shape obj tuple
    executeMessages d (getInbox obj)
updateDrawingTransformable d@(ConvexDrawing shape)    obj tuple = do
    updateTransformable shape obj tuple
    executeMessages d (getInbox obj)
updateDrawingTransformable d@(SpriteDrawing shape _)  obj tuple = do
    updateTransformable shape obj tuple
    executeMessages d (getInbox obj)
updateDrawingTransformable d@(NamedDrawing _ drw)     obj tuple = do
    updateDrawingTransformable drw obj tuple
    executeMessages d (getInbox obj)
updateDrawingTransformable (CompositeDrawing drws)  obj tuple = mapM_ (updateDrawingTransformableFlip obj tuple) drws
updateDrawingTransformable d@(TextDrawing text)       obj tuple = do
    updateTransformable text obj tuple
    executeMessages d (getInbox obj)
updateDrawingTransformable (FlaggedDrawing _ _)     _   _     = error "This pattern should not happen as the FlaggedDrawing is unwrapped on the 'updateDrawing'"

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
executeMessage (NamedDrawing nameDrw drw) (NamedMessage nameMsg f) = when (nameDrw == nameMsg) (f drw)
executeMessage (CompositeDrawing drws) msg@(NamedMessage _ _) = forM_ drws (`executeMessage` msg)
executeMessage (CompositeDrawing _) (MSG _) = return () 
executeMessage drw (MSG f) = f drw
executeMessage _ (NamedMessage _ _) = return ()