{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module ChildBearer
    ( ChildBearer
    , getChildren
    , removeChildren
    , addChild
    , addChildP
    ) where

import GameObject.GameObjectTypes (ChildGameObjectCreation, GameObjectCreation)
import Physics.PhysicsTypes (PhysicsWorld)

class ChildBearer obj st | obj -> st where
    getChildren :: obj -> [ChildGameObjectCreation st]
    addChild :: GameObjectCreation st -> obj -> obj
    addChildP :: (PhysicsWorld -> GameObjectCreation st) -> obj -> obj
    removeChildren :: obj -> obj