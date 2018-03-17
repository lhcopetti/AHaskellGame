module Component.Behavior.NoopBehavior
    ( noopBehavior
    ) where

import Component.Behavior.Behavior

noopBehavior :: BehaviorType st
noopBehavior = return
