module Component.Physics.HipMunk.VectorConversion
    ( hVectorToVec2f

    ) where

import SFML.System.Vector2 (Vec2f (..))

import qualified Physics.Hipmunk as H

hVectorToVec2f :: H.Vector -> Vec2f
hVectorToVec2f (H.Vector x y) = Vec2f (realToFrac x) (realToFrac y)