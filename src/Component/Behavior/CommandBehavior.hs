module Component.Behavior.CommandBehavior
    ( addCommandBehavior
    ) where

import GameObject.GameObjectTypes (Command (..), BehaviorType)
import GameObject.GameObject (addCommand)

addCommandBehavior :: Command st -> BehaviorType st
addCommandBehavior comm obj = return (addCommand comm obj)