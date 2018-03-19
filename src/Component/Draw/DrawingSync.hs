module Component.Draw.DrawingSync
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
import System.Messaging.DrawingInbox
import System.Messaging.Handler.RunMessageHandler (runMessages)
import GameObject.GameObjectTypes (Drawing (..))

executeUpdateOnDrawing :: (Pos.Position a, DrawingInbox a) => Drawing -> a -> (Bool, Bool) -> IO ()
executeUpdateOnDrawing drw obj tuple = do
    syncDrawingTransformable drw obj tuple
    runMessages (getInbox obj) drw


syncDrawingTransformable :: (Pos.Position a, DrawingInbox a) => Drawing -> a -> (Bool, Bool) -> IO ()
syncDrawingTransformable (CircleDrawing shape)    obj tuple = updateTransformable shape obj tuple
syncDrawingTransformable (RectangleDrawing shape) obj tuple = updateTransformable shape obj tuple
syncDrawingTransformable (ConvexDrawing shape)    obj tuple = updateTransformable shape obj tuple
syncDrawingTransformable (SpriteDrawing shape _)  obj tuple = updateTransformable shape obj tuple
syncDrawingTransformable (AnimationDrawing _ spr) obj tuple = updateTransformable spr   obj tuple
syncDrawingTransformable (TextDrawing text)       obj tuple = updateTransformable text  obj tuple
syncDrawingTransformable (NamedDrawing _ drw)     obj tuple = syncDrawingTransformable drw obj tuple
syncDrawingTransformable CompositeDrawing    {} _ _ = error "This pattern should not happen as the CompositeDrawing is unwrapped on the 'syncDrawing'"
syncDrawingTransformable FlaggedDrawing      {} _ _ = error "This pattern should not happen as the FlaggedDrawing is unwrapped on the 'syncDrawing'"
syncDrawingTransformable PhysicsDebugDrawing {} _ _ = error "This pattern should not happen as the PhysicsDebugDrawing is unwrapped on the 'syncDrawing'"
syncDrawingTransformable EmptyDrawing           _ _ = return ()

updateTransformable :: (SFTransformable a, Pos.Position b) => a -> b -> (Bool, Bool) -> IO ()
updateTransformable ptr obj (pos, rot) = do
    when pos (setPosition ptr (Pos.getPosition obj))
    when rot (setRotation ptr (Pos.getRotation obj))