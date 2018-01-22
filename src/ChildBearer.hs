module ChildBearer
    ( ChildBearer
    , getChildren
    , removeChildren
    , addChild
    ) where

import GameObject.GameObjectTypes (GameObjectCreation)

class ChildBearer a where
    getChildren :: a -> [GameObjectCreation]
    removeChildren :: a -> a
    addChild :: GameObjectCreation -> a -> a