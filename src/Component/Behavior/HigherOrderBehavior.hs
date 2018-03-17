module Component.Behavior.HigherOrderBehavior
    ( behaviorPred
    , behaveOnceAndThen
    , behaveOnce
    , behaveBoth
    , behaveAll
    , behaveEvery
    , chooseBehavior
    ) where

import Control.Monad (liftM)

import GameObject.GameObjectTypes (BehaviorType)
import Component.Behavior.Behavior (setBehaviorT)
import Component.Behavior.NoopBehavior (noopBehavior)
import GameObject.GameObject ()

-- | Sets the behavior of a gameObject given two options and a bool value.
behaviorPred :: Bool -> BehaviorType st -> BehaviorType st -> BehaviorType st
behaviorPred bool first second obj = let
    chosenBehavior = if bool then first else second
    in
        return $ setBehaviorT chosenBehavior obj

chooseBehavior :: Bool -> BehaviorType st -> BehaviorType st -> BehaviorType st
chooseBehavior pred first second obj = let
    chosenBeh = if pred then first else second
    in
        chosenBeh obj

behaveOnceAndThen :: BehaviorType st -> BehaviorType st -> BehaviorType st
behaveOnceAndThen first second obj = liftM (setBehaviorT second) (first obj)

behaveOnce :: BehaviorType st -> BehaviorType st
behaveOnce beh = behaveOnceAndThen beh noopBehavior

behaveBoth :: BehaviorType st -> BehaviorType st -> BehaviorType st
behaveBoth first second = (second =<<) . first

behaveAll :: [BehaviorType st] -> BehaviorType st
behaveAll [] obj = noopBehavior obj
behaveAll (beh:behs) obj = liftM (setBehaviorT (behaveAll behs)) (beh obj)

behaveEvery :: Int -> BehaviorType st -> BehaviorType st
behaveEvery counter = go counter counter
    where go max counter beh obj
            | counter == 0 = liftM (setBehaviorT (behaveEvery max beh)) (beh obj) 
            | otherwise = return $ setBehaviorT (go max (counter -1) beh) obj