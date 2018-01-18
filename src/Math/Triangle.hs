module Math.Triangle
    ( getUnitEqPoints
    , getUnitEqPointsScaled
    ) where

import SFML.System.Vector2 (Vec2f (..))

import Vec2.Vec2Math (multiplyScalarVec2f)

getUnitEqPoints :: [Vec2f]
getUnitEqPoints = let
    p1 = (- sqrt 3 / 4, 1 / 2)
    p2 = (sqrt 3 / 4, 0.0)
    p3 = (-sqrt 3 / 4, - 1 / 2)
    in 
        map (uncurry Vec2f) [p1, p2, p3]

getUnitEqPointsScaled :: Float -> [Vec2f]
getUnitEqPointsScaled f = map (`multiplyScalarVec2f` f) getUnitEqPoints