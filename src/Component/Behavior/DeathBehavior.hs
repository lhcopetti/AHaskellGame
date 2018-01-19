{-# LANGUAGE RankNTypes #-}
module Component.Behavior.DeathBehavior
    ( dieBehavior
    , deathByUpdates
    ) where

import Component.Behavior.Behavior
import Component.Behavior.NoopBehavior (noopBehavior)
import Killable (die)


deathByUpdates :: Int -> BehaviorType
deathByUpdates x obj
        | x < 0 = return $ setBehaviorT dieBehavior obj
        | otherwise = return $ setBehaviorT (deathByUpdates (x - 1)) obj

dieBehavior :: BehaviorType
dieBehavior = return . die