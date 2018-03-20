module Component.Input.Inputs
    ( dieOnKeyPressing
    ) where

import SFML.Window.Keyboard (KeyCode)

import Component.Input.Input (isKeyPressed)
import GameObject.GameObjectTypes

import Command.Commands (dieCommand)

dieOnKeyPressing :: KeyCode -> GoUpdateType st
dieOnKeyPressing key obj = do
    res <- isKeyPressed key
    if res then dieCommand obj else return obj