module Command.ResetCommand
    ( resetCommand
    ) where

import GameObject.GameObjectTypes (CommandType)
import GameObject.GameObject ()

import Component.Position (setPosition, setRotation)
import Component.Physics.PhysicsClass (setVelocity)
import Component.Physics.Physics ()

import Vec2.Vec2Math (zero)

resetCommand :: CommandType
resetCommand obj = let
    newObj = setPosition obj zero
    newObj' = setVelocity newObj zero
    newObj'' = setRotation 0.0 newObj'
    in
        return newObj''