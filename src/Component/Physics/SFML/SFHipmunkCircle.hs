module Component.Physics.SFML.SFHipmunkCircle
    ( mkCirclePhysics
    ) where

import SFML.System.Vector2 (Vec2f)

import qualified Component.Physics.HipMunk.HipmunkCircle as Hip
import qualified Physics.Hipmunk as H
import GameObject.GameObjectTypes (Physics (..))
import Component.Physics.HipMunk.VectorConversion (vec2fToHVector)

mkCirclePhysics :: Float -> Vec2f -> H.Space -> IO Physics
mkCirclePhysics radius pos = Hip.mkCirclePhysics hRadius hPos
    where
        hRadius = realToFrac radius
        hPos    = vec2fToHVector pos
        