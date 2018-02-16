{-# LANGUAGE NamedFieldPuns #-}
module Component.Physics.Physics
    ( Physics (..)
    , updatePosition
    ) where

import SFML.System.Vector2 (Vec2f (..))

import GameObject.GameObjectTypes
import GameObject.GameObject ()
import Vec2.Vec2Math (minVec2f, addVec2f)
import Component.Physics.PhysicsClass
import Physics.PhysicsObject (updateObjectPhysics)
import Component.Position

instance PhysicsClass Physics where
    getVelocity (SimplePhy v _) = v
    getVelocity _ = Vec2f 0 0

    setVelocity (SimplePhy _ f) v' = SimplePhy (minVec2f v' f) f
    setVelocity p _ = p

    updatePhysics = return

instance PhysicsClass GameObject where
    getVelocity = getVelocity . physicsComp
    setVelocity go@GameObject { physicsComp } newVel = go { physicsComp = setVelocity physicsComp newVel }
    updatePhysics = updatePhysicsComponent

updatePosition :: (Position a, PhysicsClass a) => a -> a
updatePosition obj = let
    pos = getPosition obj
    vel = getVelocity obj
    newPos = addVec2f pos vel
    in setPosition obj newPos

updatePhysicsComponent :: GameObject -> IO GameObject
updatePhysicsComponent go = case physicsComp go of
    SimplePhy  { } -> updateSimplePhysics go
    LibraryPhy pl  -> updateObjectPhysics pl go

updateSimplePhysics :: GameObject -> IO GameObject
updateSimplePhysics = return . updatePosition