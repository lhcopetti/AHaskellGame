module Physics.Library.Hipmunk.VectorConversion
    ( hVectorToVec2f
    , vec2fToHVector
    ) where

import SFML.System.Vector2 (Vec2f (..))

import qualified Physics.Hipmunk as H

hVectorToVec2f :: H.Vector -> Vec2f
hVectorToVec2f (H.Vector x y) = Vec2f (realToFrac x) (realToFrac y)

vec2fToHVector :: Vec2f -> H.Vector
vec2fToHVector (Vec2f x y) = H.Vector (realToFrac x) (realToFrac y)