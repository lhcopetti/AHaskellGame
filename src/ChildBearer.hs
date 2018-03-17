{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module ChildBearer
    ( ChildBearer
    , getChildren
    , removeChildren
    , addChild
    ) where

import GameObject.GameObjectTypes (GameObjectCreation)

class ChildBearer obj st | obj -> st where
    getChildren :: obj -> [GameObjectCreation st]
    addChild :: GameObjectCreation st -> obj -> obj
    removeChildren :: obj -> obj