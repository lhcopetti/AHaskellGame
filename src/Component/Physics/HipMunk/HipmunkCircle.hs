module Component.Physics.HipMunk.HipmunkCircle
    ( mkCirclePhysics
    ) where

import qualified Physics.Hipmunk as H
import Data.StateVar
import GameObject.GameObjectTypes (Physics (..))

mkCirclePhysics :: Double -> H.Vector -> H.Space -> IO Physics
mkCirclePhysics radius pos space = do
    let mass = 20
        t = H.Circle radius
    body <- newCircleBody mass radius
    shape <- H.newShape body t 0
    ----
    H.position body $= pos
    H.friction shape $= 0.5
    H.elasticity shape $= 0.9
    ----
    H.spaceAdd space body
    H.spaceAdd space shape
    return (HipPhy body shape t)


newCircleBody :: Double -> Double -> IO H.Body
newCircleBody mass radius = H.newBody mass moment
    where
        moment = H.momentForCircle mass (0, radius) 0