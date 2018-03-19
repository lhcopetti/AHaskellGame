module Physics.PhysicsMessage
    ( applyForce
    , applyOnlyForce
    , resetForces
    ) where

import SFML.System.Vector2 (Vec2f)

import qualified Physics.Library.Hipmunk.HipmunkMessage as HMP
import Physics.PhysicsTypes (PhyObject)
import Physics.Library.Hipmunk.VectorConversion (vec2fToHVector)

applyForce :: Vec2f -> Vec2f -> PhyObject -> IO ()
applyForce force offset = HMP.applyForce force' offset'
        where
            force'  = vec2fToHVector force
            offset' = vec2fToHVector offset

applyOnlyForce :: Vec2f -> Vec2f -> PhyObject -> IO ()
applyOnlyForce force offset = HMP.applyOnlyForce force' offset'
        where
            force'  = vec2fToHVector force
            offset' = vec2fToHVector offset

resetForces :: PhyObject -> IO ()
resetForces = HMP.resetForces
