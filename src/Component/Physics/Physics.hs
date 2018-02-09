module Component.Physics.Physics
    ( Physics (..)
    , updatePosition
    ) where

import SFML.System.Vector2 (Vec2f (..))

import GameObject.GameObjectTypes (Physics (..))
import Vec2.Vec2Math (minVec2f, addVec2f)
import Component.Physics.PhysicsClass
import Component.Position

instance PhysicsClass Physics where
    getVelocity (SimplePhy v _) = v
    getVelocity _ = Vec2f 0 0

    setVelocity (SimplePhy _ f) v' = SimplePhy (minVec2f v' f) f
    setVelocity p _ = p

    updatePhysics = return

updatePosition :: (Position a, PhysicsClass a) => a -> a
updatePosition obj = let
    pos = getPosition obj
    vel = getVelocity obj
    newPos = addVec2f pos vel
    in setPosition obj newPos