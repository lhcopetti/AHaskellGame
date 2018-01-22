module Component.Behavior.ChildBearerBehavior
    ( addChildBehavior
    ) where

import GameObject.GameObjectTypes (GameObjectCreation, BehaviorType)
import GameObject.GameObject ()
import qualified ChildBearer as CB

addChildBehavior :: GameObjectCreation -> BehaviorType
addChildBehavior action obj = return (CB.addChild action obj)