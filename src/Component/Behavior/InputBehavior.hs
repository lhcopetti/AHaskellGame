module Component.Behavior.InputBehavior
    ( behaveOnKeyPress
    , behaveOnKeyJustPressed
    , behaveOnMousePress
    , behaveOnMouseJustPressed
    ) where

import SFML.Window.Keyboard (KeyCode)


import GameObject.GameObjectTypes
import Component.Input.Input (isKeyPressed, isKeyJustPressed, isMousePressed, isJustPressed)
import System.Input.MouseSnapshot (MButton (..))
import Updatable (UpdateMStack)

behaveOnKeyPress :: KeyCode -> BehaviorType st -> BehaviorType st
behaveOnKeyPress = behaveOnBool isKeyPressed

behaveOnKeyJustPressed :: KeyCode -> BehaviorType st -> BehaviorType st
behaveOnKeyJustPressed = behaveOnBool isKeyJustPressed

behaveOnMousePress :: MButton -> BehaviorType st -> BehaviorType st
behaveOnMousePress = behaveOnBool isMousePressed

behaveOnMouseJustPressed :: MButton -> BehaviorType st -> BehaviorType st
behaveOnMouseJustPressed = behaveOnBool isJustPressed

behaveOnBool :: (a -> UpdateMStack Bool st) -> a -> BehaviorType st -> BehaviorType st
behaveOnBool f arg beh obj = do
    trigger <- f arg
    if trigger then beh obj else return obj