module System.GameWorld
    ( GameWorld (..)
    ) where

import SFML.Graphics.Types (RenderWindow)

import GameObject.AnyGameObject


data GameWorld = GameWorld  { window :: RenderWindow
                            , gameObjects :: [AnyGameObject]
                            }