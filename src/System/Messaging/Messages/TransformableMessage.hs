{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module System.Messaging.Messages.TransformableMessage
    ( setOriginMsg
    , setPositionMsg
    , setRotationMsg
    ) where

import SFML.System.Vector2 (Vec2f (..))
import SFML.Graphics.SFTransformable (setOrigin, setRotation, setPosition)

import GameObject.GameObjectTypes
import Component.Draw.Instances.SFTransformableDrawing ()

setOriginMsg :: Vec2f -> DrawingMessageType
setOriginMsg pos = (`setOrigin` pos)

setPositionMsg :: Vec2f -> DrawingMessageType
setPositionMsg pos = (`setPosition` pos)

setRotationMsg :: Float -> DrawingMessageType
setRotationMsg ang = (`setRotation` ang)