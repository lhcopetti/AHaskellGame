module GameObjectFactory
    ( createGameObject
    , createGameObjectWithChildren
    , createStaticGameObject
    , createStaticGameObjectB
    ) where

import SFML.System.Vector2 (Vec2f)

import Component.Draw.Drawing
import Component.Behavior.Behavior
import Component.Behavior.Behaviors (noopB)
import Component.Physics.PhysicsFactory (newEmptyPhysics, newSimplePhysics)
import Vec2.Vec2Math (zero)

import GameObject.GameObject
import GameObject.GameObjectTypes (GameObjectCreation)

createGameObjectWithChildren :: Drawing -> Behavior -> Vec2f -> Vec2f -> [GameObjectCreation] -> GameObject
createGameObjectWithChildren drw beh pos vel children = let
    live = True
    physics = newSimplePhysics vel
    initialRotation = 0.0
    in GameObject drw beh physics pos initialRotation [] children live

createGameObject :: Drawing -> Behavior -> Vec2f -> Vec2f -> GameObject
createGameObject drw beh pos vel = let
    live = True
    physics = newSimplePhysics vel
    initialRotation = 0.0
    children = []
    in GameObject drw beh physics pos initialRotation [] children live

createStaticGameObjectB :: Drawing -> Vec2f -> Behavior -> GameObject
createStaticGameObjectB drw pos beh = let 
    velocity = zero
    in
        createGameObject drw beh pos velocity

createStaticGameObject :: Drawing -> Vec2f -> GameObject
createStaticGameObject drw pos = let
    beh = noopB
    in
        createStaticGameObjectB drw pos beh