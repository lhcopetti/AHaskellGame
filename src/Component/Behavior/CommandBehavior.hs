module Component.Behavior.CommandBehavior
    ( addCommandBehavior
    ) where

import GameObject.GameObjectTypes (Command (..), BehaviorType)
import GameObject.GameObject (addCommand)

addCommandBehavior :: Command -> BehaviorType
addCommandBehavior comm obj = return (addCommand comm obj)