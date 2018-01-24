module Component.Behavior.HigherOrderBehavior
    ( behaviorPred
    , behaveOnceAndThen
    , behaveOnce
    , behaveBoth
    ) where

import Control.Monad (liftM)

import GameObject.GameObjectTypes (BehaviorType)
import Component.Behavior.Behavior (setBehaviorT)
import Component.Behavior.NoopBehavior (noopBehavior)
import GameObject.GameObject ()

-- | Sets the behavior of a gameObject given two options and a bool value.
behaviorPred :: Bool -> BehaviorType -> BehaviorType -> BehaviorType
behaviorPred bool first second obj = let
    chosenBehavior = if bool then first else second
    in
        return $ setBehaviorT chosenBehavior obj

behaveOnceAndThen :: BehaviorType -> BehaviorType -> BehaviorType
behaveOnceAndThen first second obj = liftM (setBehaviorT second) (first obj)

behaveOnce :: BehaviorType -> BehaviorType
behaveOnce beh = behaveOnceAndThen beh noopBehavior

behaveBoth :: BehaviorType -> BehaviorType -> BehaviorType
behaveBoth first second = (second =<<) . first