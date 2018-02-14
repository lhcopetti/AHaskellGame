module Math.Cross
    ( getUnitCross
    , getUnitCrossScaled
    ) where

import SFML.System.Vector2 (Vec2f (..))

import Vec2.Vec2Math (multiplyScalarVec2f)

getUnitCross :: [(Vec2f, Vec2f)]
getUnitCross =
    let hline = (Vec2f (-1) 0, Vec2f 1 0)
        vline = (Vec2f 0 (-1), Vec2f 0 1)
    in  [hline, vline]

getUnitCrossScaled :: Float -> [(Vec2f, Vec2f)]
getUnitCrossScaled f = [(multiplyScalarVec2f x f, multiplyScalarVec2f y f) | (x, y) <- getUnitCross]