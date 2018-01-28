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
    syncDrawingTransformable drw obj tuple
    executeMessages drw (getInbox obj)


syncDrawingTransformable :: (Pos.Position a, DrawingInbox a) => Drawing -> a -> (Bool, Bool) -> IO ()
syncDrawingTransformable (CircleDrawing shape)    obj tuple = updateTransformable shape obj tuple
syncDrawingTransformable (RectangleDrawing shape) obj tuple = updateTransformable shape obj tuple
syncDrawingTransformable (ConvexDrawing shape)    obj tuple = updateTransformable shape obj tuple
syncDrawingTransformable (SpriteDrawing shape _)  obj tuple = updateTransformable shape obj tuple
syncDrawingTransformable (AnimationDrawing spr)   obj tuple = updateTransformable spr   obj tuple
syncDrawingTransformable (TextDrawing text)       obj tuple = updateTransformable text  obj tuple
syncDrawingTransformable (NamedDrawing _ drw)     obj tuple = syncDrawingTransformable drw obj tuple
syncDrawingTransformable (CompositeDrawing _) _ _ = error "This pattern should not happen as the CompositeDrawing is unwrapped on the 'syncDrawing'"
syncDrawingTransformable (FlaggedDrawing _ _) _ _ = error "This pattern should not happen as the FlaggedDrawing is unwrapped on the 'syncDrawing'"

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