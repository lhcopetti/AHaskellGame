module Component.Draw.DrawingData
    ( Drawing (..)
    ) where

import SFML.Graphics.Types (CircleShape, RectangleShape, ConvexShape, Text, Sprite, Texture)

data Drawing = CircleDrawing CircleShape
             | RectangleDrawing RectangleShape
             | ConvexDrawing ConvexShape
             | TextDrawing Text
             | SpriteDrawing Sprite Texture
             | CompositeDrawing [Drawing]