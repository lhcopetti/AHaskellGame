module Component.Behavior.CollisionBehavior
    ( getCollisionPointCount
    ) where

import SFML.System.Vector2 (Vec2f)

import Control.Monad.Reader (asks)

import GameEnv (GameEnvironment (..))

import Updatable
import Component.Physics.Physics ()
import Physics.PhysicsCollision (collisionPoints)


getCollisionPointCount :: UpdateMStack [Vec2f] st
getCollisionPointCount = asks (collisionPoints . collisionData)