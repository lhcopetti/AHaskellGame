module Component.Draw.DrawingUpdate
    ( executeUpdateOnDrawing
    ) where

import Control.Monad (when)

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

executeUpdateOnDrawing :: (Pos.Position a, DrawingInbox a) => Drawing -> a -> (Bool, Bool) -> IO ()
executeUpdateOnDrawing drw obj tuple = do
    updateDrawingTransformable drw obj tuple
    executeMessages drw (getInbox obj)


updateDrawingTransformable :: (Pos.Position a, DrawingInbox a) => Drawing -> a -> (Bool, Bool) -> IO ()
updateDrawingTransformable (CircleDrawing shape)    obj tuple = updateTransformable shape obj tuple
updateDrawingTransformable (RectangleDrawing shape) obj tuple = updateTransformable shape obj tuple
updateDrawingTransformable (ConvexDrawing shape)    obj tuple = updateTransformable shape obj tuple
updateDrawingTransformable (SpriteDrawing shape _)  obj tuple = updateTransformable shape obj tuple
updateDrawingTransformable (AnimationDrawing spr)   obj tuple = updateTransformable spr   obj tuple
updateDrawingTransformable (TextDrawing text)       obj tuple = updateTransformable text  obj tuple
updateDrawingTransformable (NamedDrawing _ drw)     obj tuple = updateDrawingTransformable drw obj tuple
updateDrawingTransformable (CompositeDrawing _) _ _ = error "This pattern should not happen as the CompositeDrawing is unwrapped on the 'updateDrawing'"
updateDrawingTransformable (FlaggedDrawing _ _) _ _ = error "This pattern should not happen as the FlaggedDrawing is unwrapped on the 'updateDrawing'"

updateTransformable :: (SFTransformable a, Pos.Position b) => a -> b -> (Bool, Bool) -> IO ()
updateTransformable ptr obj (pos, rot) = do
    when pos (setPosition ptr (Pos.getPosition obj))
    when rot (setRotation ptr (Pos.getRotation obj))

executeMessages :: Drawing -> [DrawingMessage] -> IO ()
executeMessages drw = mapM_ (executeMessage drw)

executeMessage :: Drawing -> DrawingMessage -> IO ()
executeMessage (NamedDrawing nameDrw drw)   (NamedMessage nameMsg f) = when (nameDrw == nameMsg) (f drw)
executeMessage _                            (NamedMessage _ _)       = return ()
executeMessage drw (MSG f) = f drw