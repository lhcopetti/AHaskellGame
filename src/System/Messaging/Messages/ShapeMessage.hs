{-# LANGUAGE RankNTypes #-}
module System.Messaging.Messages.ShapeMessage
    ( setFillColorMsg
    , setOutlineColorMsg
    , setOutlineThicknessMsg
    ) where

import SFML.Graphics.SFShape
import SFML.Graphics.Color

import Component.Draw.Instances.SFShapeDrawing ()
import GameObject.GameObjectTypes

setFillColorMsg :: Color -> DrawingMessageType
setFillColorMsg c = (`setFillColor` c)

setOutlineColorMsg :: Color -> DrawingMessageType
setOutlineColorMsg c = (`setOutlineColor` c)

setOutlineThicknessMsg :: Float -> DrawingMessageType
setOutlineThicknessMsg f = (`setOutlineThickness` f)