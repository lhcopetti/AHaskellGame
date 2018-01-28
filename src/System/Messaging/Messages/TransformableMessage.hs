module System.Messaging.Messages.TransformableMessage
    ( setOriginMsg
    ) where

import SFML.System.Vector2 (Vec2f)
import SFML.Graphics.CircleShape ()
import SFML.Graphics.RectangleShape ()
import SFML.Graphics.ConvexShape ()
import SFML.Graphics.Text ()
import SFML.Graphics.Sprite ()
import SFML.Graphics.Texture ()
import SFML.Graphics.SFTransformable (setOrigin)

import GameObject.GameObjectTypes (Drawing (..), DrawingMessageType)

setOriginMsg :: Vec2f -> DrawingMessageType
setOriginMsg pos (CircleDrawing     ptr) = setOrigin    ptr pos
setOriginMsg pos (RectangleDrawing  ptr) = setOrigin    ptr pos
setOriginMsg pos (ConvexDrawing     ptr) = setOrigin    ptr pos
setOriginMsg pos (TextDrawing       ptr) = setOrigin    ptr pos
setOriginMsg pos (SpriteDrawing   ptr _) = setOrigin    ptr pos
setOriginMsg pos (AnimationDrawing  ptr) = setOrigin    ptr pos
setOriginMsg pos (FlaggedDrawing  ptr _) = setOriginMsg pos ptr
setOriginMsg pos (NamedDrawing   _  drw) = setOriginMsg pos drw
setOriginMsg pos (CompositeDrawing drws) = mapM_ (pos `setOriginMsg`) drws