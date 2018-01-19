module Component.Draw.DrawingData
    ( Drawing (..)
    ) where

import SFML.Graphics.Types (CircleShape, RectangleShape, ConvexShape, Text)

data Drawing = CircleDrawing CircleShape
             | RectangleDrawing RectangleShape
             | ConvexDrawing ConvexShape
             | TextDrawing Text
             | CompositeDrawing [Drawing]