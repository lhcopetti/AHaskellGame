module Component.Draw.DrawingData
    ( Drawing (..)
    , DrawingFlag (..)
    ) where

import SFML.Graphics.Types (CircleShape, RectangleShape, ConvexShape, Text, Sprite, Texture)

data Drawing = CircleDrawing CircleShape
             | RectangleDrawing RectangleShape
             | ConvexDrawing ConvexShape
             | TextDrawing Text
             | SpriteDrawing Sprite Texture
             | CompositeDrawing [Drawing]
             | FlaggedDrawing Drawing [DrawingFlag]
             | NamedDrawing String Drawing
             | AnimationDrawing Sprite

data DrawingFlag = NoRotationUpdates
                  | NoPositionUpdates
                  deriving (Eq)