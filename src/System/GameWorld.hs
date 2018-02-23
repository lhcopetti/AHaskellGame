{-# LANGUAGE NamedFieldPuns #-}
module System.GameWorld
    ( GameWorld (..)
    , GameScene (..)
    , adoptChildren
    ) where

import SFML.Graphics.Types (RenderWindow)

import System.GameScene (GameScene (..))
import GameObject.AnyGameObject


data GameWorld = GameWorld  { window :: RenderWindow
                            }

-- | Puts the newly created child objects along with the standard gameObjects collection. 
-- This two-part system to add child objects to the GameWorld happens because 
-- we don't want to synchronize or draw objects that have not had their first update
-- cycle yet.
adoptChildren :: GameScene -> [AnyGameObject] -> GameScene
adoptChildren scene@ GameScene { gameObjects } orphanChildren = scene { gameObjects = gameObjects ++ orphanChildren }