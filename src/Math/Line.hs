module Math.Line
    ( getRectFromLine
    ) where

import SFML.System.Vector2 (Vec2f)

import Vec2.Vec2Math

getRectFromLine :: (Vec2f, Vec2f) -> Float -> [Vec2f]
getRectFromLine (start, end) thickness = let
    (orthoDown, orthoUp) = getOrthoVec2f $ unitVec2f (subtractVec2f end start)
    thickEachSide = thickness / 2.0
    topLeft     = start    |+|      (multiplyScalarVec2f orthoUp    thickEachSide)
    topRight    = end      |+|      (multiplyScalarVec2f orthoUp    thickEachSide)
    bottomRight = end      |+|      (multiplyScalarVec2f orthoDown  thickEachSide)
    bottomLeft  = start    |+|      (multiplyScalarVec2f orthoDown  thickEachSide)
    in
        [topLeft, topRight, bottomRight, bottomLeft]
