module Component.Input.Inputs
    ( dieOnKeyPressing
    ) where

import SFML.Window.Keyboard (KeyCode)

import Component.Input.Input (isPressed)
import GameObject.GameObjectTypes (GameObject, InputType)

import Command.Commands (dieCommand)

dieOnKeyPressing :: KeyCode -> InputType GameObject
dieOnKeyPressing key obj = do
    res <- isPressed key
    if res then dieCommand obj else return obj