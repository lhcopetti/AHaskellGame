{-# LANGUAGE NamedFieldPuns #-}
module System.GameWorld
    ( GameWorld (..)
    , GameScene (..)
    , adoptChildren
    ) where

import SFML.Graphics.Types (RenderWindow)
import SFML.Graphics.RenderWindow (destroy)

import GameObject.GameObjectTypes
import System.GameScene (GameScene (..))
import NativeResource


data GameWorld = GameWorld  { window :: RenderWindow
                            }

-- | Puts the newly created child objects along with the standard gameObjects collection. 
-- This two-part system to add child objects to the GameWorld happens because 
-- we don't want to synchronize or draw objects that have not had their first update
-- cycle yet.
adoptChildren :: GameScene -> [GameObject GoVoidState] -> GameScene
adoptChildren scene@ GameScene { gameObjects } orphanChildren = scene { gameObjects = gameObjects ++ orphanChildren }


instance NativeResource GameWorld where
    free (GameWorld window) = destroy window