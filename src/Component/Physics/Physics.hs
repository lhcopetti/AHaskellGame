module Component.Physics.Physics
    ( Physics (..)
    , updatePhysics
    ) where

import SFML.System.Vector2 (Vec2f (..))

import qualified Physics.Hipmunk as H

import Vec2.Vec2Math (minVec2f, addVec2f)
import Component.Physics.PhysicsClass
import Component.Position

data Physics = SimplePhy Vec2f Float
             | HipPhy H.Body H.Shape H.ShapeType

instance PhysicsClass Physics where
    getVelocity (SimplePhy v _) = v
    getVelocity _ = Vec2f 0 0

    setVelocity (SimplePhy _ f) v' = SimplePhy (minVec2f v' f) f
    setVelocity p _ = p

updatePhysics :: (Position a, PhysicsClass a) => a -> a
updatePhysics obj = let
    pos = getPosition obj
    vel = getVelocity obj
    newPos = updatePosition pos vel
    in setPosition obj newPos

updatePosition :: Vec2f -> Vec2f -> Vec2f
updatePosition = addVec2f