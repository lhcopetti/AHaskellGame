module Physics.Library.Hipmunk.HipmunkPolygon
    ( mkPolygonPhysics
    ) where

import qualified Physics.Hipmunk as H
import Data.StateVar
import GameObject.GameObjectTypes
import Physics.PhysicsTypes (PhysicsWorld (..))

mkPolygonPhysics :: H.Vector -> [H.Vector] -> PhysicsWorld -> IO Physics
mkPolygonPhysics pos points (PhysicsWorld space) = do
    let mass = 50
        t = H.Polygon points
    body <- H.newBody mass $ H.momentForShape mass t 0
    shape <- H.newShape body t 0
    ----
    H.position body $= pos
    H.friction shape $= 1
    H.elasticity shape $= 0.9
    ----
    H.spaceAdd space body
    H.spaceAdd space shape
    ----
    let remove = H.spaceRemove space body >> H.spaceRemove space shape
    return (HipPhy body shape t remove)