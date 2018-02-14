module Math.Hexagon
    ( getUnitHexPoints
    , getUnitHexPointsScaled
    ) where

import SFML.System.Vector2 (Vec2f (..))

import Vec2.Vec2Math (multiplyScalarVec2f)

-- |This function returns a list containing the points for a zero centered, unit (1) sided hexagon
getUnitHexPoints :: [Vec2f]
getUnitHexPoints = let
    p1 = (-0.5, sqrt 3 / 2)
    p2 = (0.5, sqrt 3 / 2)
    p3 = (1, 0)
    p4 = (0.5, - sqrt 3 / 2)
    p5 = (-0.5, - sqrt 3 / 2)
    p6 = (-1, 0)
    in map (uncurry Vec2f) [p1, p2, p3, p4, p5, p6]

getUnitHexPointsScaled :: Float -> [Vec2f]
getUnitHexPointsScaled f = map (`multiplyScalarVec2f` f) getUnitHexPoints
