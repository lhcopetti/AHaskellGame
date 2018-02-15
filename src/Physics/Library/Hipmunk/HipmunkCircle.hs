module Physics.Library.Hipmunk.HipmunkCircle
    ( mkCirclePhysics
    ) where

import qualified Physics.Hipmunk as H
import Data.StateVar
import GameObject.GameObjectTypes
import Physics.PhysicsTypes (PhysicsWorld (..))

mkCirclePhysics :: Double -> H.Vector -> PhysicsWorld -> IO Physics
mkCirclePhysics radius pos (PhysicsWorld space) = do
    let mass = 20
        t = H.Circle radius
    body <- newCircleBody mass radius
    shape <- H.newShape body t 0
    ----
    H.position body $= pos
    H.friction shape $= 0.5
    H.elasticity shape $= 0.4
    ----
    H.spaceAdd space body
    H.spaceAdd space shape
    ----
    let remove = H.spaceRemove space body >> H.spaceRemove space shape
    return (HipPhy $ PL body shape t remove)

newCircleBody :: Double -> Double -> IO H.Body
newCircleBody mass radius = H.newBody mass moment
    where
        moment = H.momentForCircle mass (0, radius) 0