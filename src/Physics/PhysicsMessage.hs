module Physics.PhysicsMessage
    ( applyForce
    , applyOnlyForce
    , applyTorque
    , setAngleVelocity
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

applyTorque :: Float -> PhyObject -> IO ()
applyTorque = HMP.applyTorque . realToFrac

applyOnlyForce :: Vec2f -> Vec2f -> PhyObject -> IO ()
applyOnlyForce force offset = HMP.applyOnlyForce force' offset'
        where
            force'  = vec2fToHVector force
            offset' = vec2fToHVector offset

setAngleVelocity :: Float -> PhyObject -> IO ()
setAngleVelocity = HMP.setAngleVelocity . realToFrac

resetForces :: PhyObject -> IO ()
resetForces = HMP.resetForces
