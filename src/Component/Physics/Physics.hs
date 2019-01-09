{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}
module Component.Physics.Physics
    ( Physics (..)
    , updatePosition
    ) where

import SFML.System.Vector2 (Vec2f (..))

import GameObject.GameObjectTypes
import GameObject.GameObject ()
import Vec2.Vec2Math (minVec2f, (|+|))
import Component.Physics.PhysicsClass
import Physics.PhysicsObject (updateObjectPhysics)
import Component.Position

instance PhysicsClass Physics where
    getVelocity (SimplePhy v _) = v
    getVelocity _ = Vec2f 0 0

    setVelocity (SimplePhy _ f) v' = SimplePhy (minVec2f v' f) f
    setVelocity p _ = p

    updatePhysics = return

instance PhysicsClass (GameObject a) where
    getVelocity = getVelocity . physicsComp
    setVelocity go@GameObject { physicsComp } newVel = go { physicsComp = setVelocity physicsComp newVel }
    updatePhysics = updatePhysicsComponent

updatePosition :: (Position a, PhysicsClass a) => a -> a
updatePosition obj = let
    pos = getPosition obj
    vel = getVelocity obj
    newPos = pos |+| vel
    in setPosition obj newPos

updatePhysicsComponent :: GameObject a -> IO (GameObject a)
updatePhysicsComponent go = case physicsComp go of
    SimplePhy  { } -> updateSimplePhysics go
    LibraryPhy pl  -> updateObjectPhysics pl go

updateSimplePhysics :: GameObject a -> IO (GameObject a)
updateSimplePhysics = return . updatePosition
