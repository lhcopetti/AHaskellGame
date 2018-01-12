module Component.Behavior.Behaviors
    ( encloseToBoxB
    ) where

import Component.Behavior.Behavior
import Component.Behavior.EnclosedBehavior (encloseToBox)

encloseToBoxB :: Behavior
encloseToBoxB = Behavior encloseToBox