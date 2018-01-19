module Component.Behavior.DeathBehavior
    ( dieBehavior
    ) where

import Component.Behavior.Behavior
import Killable (die)

dieBehavior :: BehaviorType
dieBehavior = return . die