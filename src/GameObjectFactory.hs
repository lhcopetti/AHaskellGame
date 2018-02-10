module GameObjectFactory
    ( createGameObject
    , createStaticGameObject
    , createStaticGameObjectB
    ) where

import SFML.System.Vector2 (Vec2f)

import Component.Draw.Drawing
import Component.Behavior.Behavior
import Component.Behavior.Behaviors (noopB)
import Component.Physics.PhysicsFactory (newSimplePhysics)
import Component.Input.Input (emptyInput)
import Vec2.Vec2Math (zero)

import GameObject.GameObject

createGameObject :: Drawing -> Behavior -> Vec2f -> Vec2f -> GameObject
createGameObject drw beh pos vel = let
    live = True
    physics = newSimplePhysics vel
    initialRotation = 0.0
    children = []
    noCommands = []
    input = emptyInput
    in GameObject drw beh physics input pos initialRotation [] children noCommands live    

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