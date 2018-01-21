module ChildBearer
    ( ChildBearer
    , getChildren
    , removeChildren
    ) where

import GameObject.GameObjectTypes (GameObjectCreation)

class ChildBearer a where
    getChildren :: a -> [GameObjectCreation]
    removeChildren :: a -> a