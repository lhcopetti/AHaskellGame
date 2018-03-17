module Component.Behavior.ChildBearerBehavior
    ( addChildBehavior
    ) where

import GameObject.GameObjectTypes (GameObjectCreation, BehaviorType)
import GameObject.GameObject ()
import qualified ChildBearer as CB

addChildBehavior :: GameObjectCreation st -> BehaviorType st
addChildBehavior action obj = return (CB.addChild action obj)