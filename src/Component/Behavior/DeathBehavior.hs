{-# LANGUAGE RankNTypes #-}
module Component.Behavior.DeathBehavior
    ( dieBehavior
    , deathByUpdates
    , deathByHitsOnWall
    , deathByOutOfBounds
    ) where

import Component.Behavior.Behavior
import Component.Behavior.EnclosedBehavior (encloseToBox)
import Component.Behavior.HigherOrderBehavior (behaviorPred)

import Component.Physics.PhysicsClass
import ExtSFML.SFMLInstances ()

import Killable (die)


deathByHitsOnWall :: Int -> BehaviorType st
deathByHitsOnWall x obj
        | x < 0 = return $ setBehaviorT dieBehavior obj
        | otherwise = do
            newObj <- encloseToBox obj
            let oldVel = getVelocity obj
            let newVel = getVelocity newObj
            let newCount = if oldVel /= newVel then x - 1 else x
            return $ setBehaviorT (deathByHitsOnWall newCount) newObj

deathByUpdates :: Int -> BehaviorType st
deathByUpdates x = behaviorPred (x < 0) dieBehavior (deathByUpdates (x - 1))

dieBehavior :: BehaviorType st
dieBehavior = return . die

deathByOutOfBounds :: BehaviorType st
deathByOutOfBounds = undefined