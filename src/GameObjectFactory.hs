module GameObjectFactory
    ( createGameObject
    , createStaticGameObject
    ) where

import SFML.System.Vector2 (Vec2f)

import Component.Draw.Drawing
import Component.Behavior.Behavior
import Component.Behavior.Behaviors (noopB)
import Component.Physics.PhysicsFactory (newEmptyPhysics, newSimplePhysics)
import Vec2.Vec2Math (zero)

import GameObject.Ball

createGameObject :: Drawing -> Behavior -> Vec2f -> Vec2f -> Ball
createGameObject drw beh pos vel = let
    alive = True
    physics = newSimplePhysics vel
    initialRotation = 0.0
    in Ball drw beh physics pos initialRotation alive

createStaticGameObject :: Drawing -> Vec2f -> Ball
createStaticGameObject drw pos = let
    behavior = noopB
    velocity = zero
    in
        createGameObject drw behavior pos velocity