module Component.Behavior.InputBehavior
    ( behaveOnKeyPress
    , behaveOnKeyJustPressed
    ) where

import SFML.Window.Keyboard (KeyCode)

import GameObject.GameObjectTypes
import Component.Input.Input (isKeyPressed, isKeyJustPressed)

behaveOnKeyPress :: KeyCode -> BehaviorType st -> BehaviorType st
behaveOnKeyPress key beh obj = do
    res <- isKeyPressed key
    if res then beh obj else return obj

behaveOnKeyJustPressed :: KeyCode -> BehaviorType st -> BehaviorType st
behaveOnKeyJustPressed key beh obj = do
    res <- isKeyJustPressed key
    if res then beh obj else return obj