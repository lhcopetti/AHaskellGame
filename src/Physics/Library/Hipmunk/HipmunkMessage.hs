module Physics.Library.Hipmunk.HipmunkMessage
    ( applyForce
    ) where

import qualified Physics.Hipmunk as H
import Physics.Library.Hipmunk.PhysicsTypes

applyForce :: H.Vector -> H.Position -> PhysicsObject -> IO ()
applyForce force offset obj = H.applyForce (body obj) force offset