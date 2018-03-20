module Component.Behavior.InputBehavior
    ( behaveOnKeyPress
    ) where

import SFML.Window.Keyboard (KeyCode)

import GameObject.GameObjectTypes
import Component.Input.Input (isKeyPressed)

behaveOnKeyPress :: KeyCode -> BehaviorType st -> BehaviorType st
behaveOnKeyPress key beh obj = do
    res <- isKeyPressed key
    if res then beh obj else return obj