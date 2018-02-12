module Math.Square
    ( unitSquarePoints
    , unitSquarePointsScaled
    ) where

import SFML.System.Vector2 (Vec2f (..))

import Vec2.Vec2Math (multiplyScalarVec2f)

unitSquarePoints :: [Vec2f]
unitSquarePoints = let
    sides = [(-1, 1), (1, 1), (1, -1), (-1, -1)]
    in 
        map (uncurry Vec2f) sides

unitSquarePointsScaled :: Float -> [Vec2f]
unitSquarePointsScaled f = map (`multiplyScalarVec2f` f) unitSquarePoints