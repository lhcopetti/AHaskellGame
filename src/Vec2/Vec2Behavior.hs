module Vec2.Vec2Behavior 
    ( direction
    ) where

import SFML.System.Vector2 (Vec2f)

import Vec2.Vec2Math (subtractVec2f, unitVec2f)

direction :: Vec2f -> Vec2f -> Vec2f
direction target origin = unitVec2f (subtractVec2f target origin)