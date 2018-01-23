module Component.Behavior.HigherOrderBehavior
    ( behaviorPred
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

behaveOnce :: BehaviorType -> BehaviorType
behaveOnce beh obj = liftM (setBehaviorT noopBehavior) (beh obj)

behaveBoth :: BehaviorType -> BehaviorType -> BehaviorType
behaveBoth first second = (second =<<) . first