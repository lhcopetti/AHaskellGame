module Physics.Library.Hipmunk.HipmunkMessage
    ( applyForce
    , applyOnlyForce
    , resetForces
    ) where

import qualified Physics.Hipmunk as H
import Physics.Library.Hipmunk.PhysicsTypes

applyForce :: H.Vector -> H.Position -> PhysicsObject -> IO ()
applyForce force offset obj = H.applyForce (body obj) force offset

applyOnlyForce :: H.Vector -> H.Position -> PhysicsObject -> IO ()
applyOnlyForce force offset obj = H.applyOnlyForce (body obj) force offset

resetForces :: PhysicsObject -> IO ()
resetForces = H.resetForces . body