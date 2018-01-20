{-# LANGUAGE RankNTypes #-}
module Component.Behavior.DeathBehavior
    ( dieBehavior
    , deathByUpdates
    , deathByHitsOnWall
    ) where

import Component.Behavior.Behavior
import Component.Behavior.NoopBehavior (noopBehavior)
import Component.Behavior.EnclosedBehavior (encloseToBox)
import GameObject.GameObjectTypes

import Component.Physics.PhysicsClass
import Vec2.Vec2Instances

import Killable (die)


deathByHitsOnWall :: Int -> BehaviorType
deathByHitsOnWall x obj
        | x < 0 = return $ setBehaviorT dieBehavior obj
        | otherwise = do
            newObj <- encloseToBox obj
            let oldVel = getVelocity obj
            let newVel = getVelocity newObj
            let newCount = if oldVel /= newVel then x - 1 else x
            return $ setBehaviorT (deathByHitsOnWall newCount) newObj

deathByUpdates :: Int -> BehaviorType
deathByUpdates x = behaviorPred (x < 0) dieBehavior (deathByUpdates (x - 1))

behaviorPred :: Bool -> BehaviorType -> BehaviorType -> BehaviorType
behaviorPred bool fst snd obj = let
    chosenBehavior = if bool then fst else snd
    in
        return $ setBehaviorT chosenBehavior obj

dieBehavior :: BehaviorType
dieBehavior = return . die