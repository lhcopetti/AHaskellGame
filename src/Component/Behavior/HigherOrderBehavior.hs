module Component.Behavior.HigherOrderBehavior
    ( behaviorPred
    ) where

import GameObject.GameObjectTypes (BehaviorType)
import Component.Behavior.Behavior (setBehaviorT)
import GameObject.GameObject ()

-- | Sets the behavior of a gameObject given two options and a bool value.
behaviorPred :: Bool -> BehaviorType -> BehaviorType -> BehaviorType
behaviorPred bool fst snd obj = let
    chosenBehavior = if bool then fst else snd
    in
        return $ setBehaviorT chosenBehavior obj
