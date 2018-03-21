module Physics.Library.Hipmunk.HipmunkMessage
    ( applyForce
    , applyOnlyForce
    , applyTorque
    , setAngleVelocity
    , resetForces
    ) where

import qualified Data.StateVar as SV

import qualified Physics.Hipmunk as H
import Physics.Library.Hipmunk.PhysicsTypes

applyForce :: H.Vector -> H.Position -> PhysicsObject -> IO ()
applyForce force offset obj = H.applyForce (body obj) force offset

applyOnlyForce :: H.Vector -> H.Position -> PhysicsObject -> IO ()
applyOnlyForce force offset obj = H.applyOnlyForce (body obj) force offset

applyTorque :: Double -> PhysicsObject -> IO ()
applyTorque f = (SV.$= f) . H.torque . body

setAngleVelocity :: Double -> PhysicsObject -> IO ()
setAngleVelocity f = (SV.$= f) . H.angVel . body

resetForces :: PhysicsObject -> IO ()
resetForces = H.resetForces . body