{-# LANGUAGE NamedFieldPuns #-}
module System.GameWorld
    ( GameWorld (..)
    , adoptChildren
    ) where

import SFML.Graphics.Types (RenderWindow)

import qualified Physics.Hipmunk as H

import GameObject.AnyGameObject


data GameWorld = GameWorld  { space :: H.Space
                            , window :: RenderWindow
                            , gameObjects :: [AnyGameObject]
                            }

-- | Puts the newly created child objects along with the standard gameObjects collection. 
-- This two-part system to add child objects to the GameWorld happens because 
-- we don't want to synchronize or draw objects that have not had their first update
-- cycle yet.
adoptChildren :: GameWorld -> [AnyGameObject] -> GameWorld
adoptChildren world@ GameWorld { gameObjects } orphanChildren = world { gameObjects = gameObjects ++ orphanChildren }