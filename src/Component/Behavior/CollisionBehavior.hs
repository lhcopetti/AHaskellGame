module Component.Behavior.CollisionBehavior
    ( getCollisionPointCount
    , hasCollided
    ) where

import SFML.System.Vector2 (Vec2f)

import Control.Monad.Reader (asks)

import GameEnv (GameEnvironment (..))

import GameObject.GameObjectTypes
import Updatable
import Component.Physics.Physics ()
import qualified Physics.PhysicsCollision as PC (collisionPoints, hasCollided)


getCollisionPointCount :: UpdateMStack [Vec2f] st
getCollisionPointCount = asks (PC.collisionPoints . collisionData)

hasCollided :: GameObject st -> UpdateMStack Bool st
hasCollided obj = do
    collData <- asks collisionData
    let res = case physicsComp obj of 
            SimplePhy {} -> False
            LibraryPhy p -> PC.hasCollided p collData
    return res