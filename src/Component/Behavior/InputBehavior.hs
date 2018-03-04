module Component.Behavior.InputBehavior
    ( behaveOnKeyPress
    ) where

import SFML.Window.Keyboard (KeyCode)

import GameObject.GameObjectTypes
import Component.Input.Input (isPressed)

behaveOnKeyPress :: KeyCode -> BehaviorType -> BehaviorType
behaveOnKeyPress key beh obj = do
    res <- isPressed key
    if res then beh obj else return obj