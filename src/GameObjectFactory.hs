module GameObjectFactory
    ( createGameObject
    , createSimplePhysicsGO
    , createStaticGameObject
    , createStaticGameObjectB
    , createLogicGameObject
    ) where

import SFML.System.Vector2 (Vec2f)

import GameObject.GameObjectTypes
import Component.Draw.ZDrawing
import Component.Behavior.Behaviors (noopB)
import Component.Physics.PhysicsFactory (newSimplePhysics)
import Component.Input.Input (emptyInput)
import Vec2.Vec2Math (zero)

createGameObject :: Drawing -> Behavior st -> Physics -> Vec2f -> GameObject st
createGameObject drw beh physics pos = let
    live = True
    initialRotation = 0.0
    children = []
    noCommands = []
    input = emptyInput
    emptyInbox = []
    emptyPhysicsInbox = []
    zdrw = mkZDrawing drw
    in GameObject zdrw beh physics input pos initialRotation emptyInbox emptyPhysicsInbox children noCommands live

createSimplePhysicsGO :: Drawing -> Behavior st -> Vec2f -> Vec2f -> GameObject st
createSimplePhysicsGO drw beh pos vel = 
    createGameObject drw beh physics pos
        where physics = newSimplePhysics vel

createStaticGameObjectB :: Drawing -> Vec2f -> Behavior st -> GameObject st
createStaticGameObjectB drw pos beh = let 
    velocity = zero
    in
        createSimplePhysicsGO drw beh pos velocity

createStaticGameObject :: Drawing -> Vec2f -> GameObject st
createStaticGameObject drw pos = let
    beh = noopB
    in
        createStaticGameObjectB drw pos beh

createLogicGameObject :: Behavior st -> GameObject st
createLogicGameObject = createStaticGameObjectB EmptyDrawing zero