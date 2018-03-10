module GameObjectFactory
    ( createGameObject
    , createSimplePhysicsGO
    , createStaticGameObject
    , createStaticGameObjectB
    , createLogicGameObject
    ) where

import SFML.System.Vector2 (Vec2f)

import GameObject.GameObjectTypes (Physics)
import Component.Draw.Drawing
import Component.Draw.ZDrawing
import Component.Behavior.Behavior
import Component.Behavior.Behaviors (noopB)
import Component.Physics.PhysicsFactory (newSimplePhysics)
import Component.Input.Input (emptyInput)
import Vec2.Vec2Math (zero)

import GameObject.GameObject

createGameObject :: Drawing -> Behavior -> Physics -> Vec2f -> GameObject
createGameObject drw beh physics pos = let
    live = True
    initialRotation = 0.0
    children = []
    noCommands = []
    input = emptyInput
    emptyInbox = []
    zdrw = mkZDrawing drw
    in GameObject zdrw beh physics input pos initialRotation emptyInbox children noCommands live

createSimplePhysicsGO :: Drawing -> Behavior -> Vec2f -> Vec2f -> GameObject
createSimplePhysicsGO drw beh pos vel = 
    createGameObject drw beh physics pos
        where physics = newSimplePhysics vel

createStaticGameObjectB :: Drawing -> Vec2f -> Behavior -> GameObject
createStaticGameObjectB drw pos beh = let 
    velocity = zero
    in
        createSimplePhysicsGO drw beh pos velocity

createStaticGameObject :: Drawing -> Vec2f -> GameObject
createStaticGameObject drw pos = let
    beh = noopB
    in
        createStaticGameObjectB drw pos beh

createLogicGameObject :: Behavior -> GameObject
createLogicGameObject = createStaticGameObjectB EmptyDrawing zero