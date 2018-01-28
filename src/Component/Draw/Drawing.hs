{-# LANGUAGE RecordWildCards #-}
module Component.Draw.Drawing
    ( Drawing (..)
    , updateDrawing
    , syncDrawing
    , destroyDrawing
    ) where

import SFML.Graphics.RenderWindow (drawCircle, drawRectangle, drawConvexShape, drawText, drawSprite)
import SFML.SFResource (destroy)

import qualified Component.Position as Pos
import Component.Draw.DrawingUpdate (executeUpdateOnDrawing)
import Drawable
import System.Messaging.DrawingMessage
import GameObject.GameObjectTypes (GameObject (..), Drawing (..), DrawingFlag (..))
import Component.Animation.Animation (destroyAnimation)
import Component.Animation.AnimationDrawing (updateAnimation)

instance Drawable Drawing where
    draw wnd (CircleDrawing ptr) = drawCircle wnd ptr Nothing
    draw wnd (RectangleDrawing ptr) = drawRectangle wnd ptr Nothing
    draw wnd (ConvexDrawing ptr) = drawConvexShape wnd ptr Nothing
    draw wnd (TextDrawing ptr) = drawText wnd ptr Nothing
    draw wnd (SpriteDrawing sprite _) = drawSprite wnd sprite Nothing
    draw wnd (AnimationDrawing sprite) = drawSprite wnd sprite Nothing
    draw wnd (NewAnimationDrawing _ sprite) = drawSprite wnd sprite Nothing
    draw wnd (FlaggedDrawing drawing _) = draw wnd drawing
    draw wnd (CompositeDrawing drws) = mapM_ (draw wnd) drws
    draw wnd (NamedDrawing _ drw) = draw wnd drw

updateDrawing :: GameObject -> GameObject
updateDrawing obj@GameObject{..} = obj { drawComp = update drawComp }

update :: Drawing -> Drawing
update (NewAnimationDrawing anim spr) = updateAnimation anim spr
update d = d

syncDrawing :: (Pos.Position a, DrawingInbox a) => Drawing -> a -> IO ()
syncDrawing (FlaggedDrawing drw flg) obj  = executeUpdateOnDrawing drw obj (updatePosition, updateRotation)
    where
        updatePosition = NoPositionUpdates `notElem` flg
        updateRotation = NoRotationUpdates `notElem` flg
syncDrawing (CompositeDrawing drws)  obj  = mapM_ (`syncDrawing` obj) drws
syncDrawing drw obj                       = executeUpdateOnDrawing drw obj (True, True)

destroyDrawing :: Drawing -> IO ()
destroyDrawing (CircleDrawing       ptr ) = destroy ptr
destroyDrawing (RectangleDrawing    ptr ) = destroy ptr
destroyDrawing (ConvexDrawing       ptr ) = destroy ptr
destroyDrawing (TextDrawing         ptr ) = destroy ptr
destroyDrawing (SpriteDrawing   spr tex ) = destroy spr >> destroy tex
destroyDrawing (AnimationDrawing    _   ) = return ()
        -- The sprite destruction for animations should not be handled here, as it is destroyed
        -- by the gameObject itself.
destroyDrawing (NewAnimationDrawing anim _) = destroyAnimation anim
destroyDrawing (CompositeDrawing    drws) = mapM_ destroyDrawing drws
destroyDrawing (FlaggedDrawing    ptr _ ) = destroyDrawing ptr
destroyDrawing (NamedDrawing      _ ptr ) = destroyDrawing ptr