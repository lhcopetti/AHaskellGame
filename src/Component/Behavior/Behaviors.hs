module Component.Behavior.Behaviors
    ( encloseToBoxB
    , encloseByWrapAroundB
    , noopB
    , deadManWalkingB
    , mousePositionCopierB
    , mouseFollowerB
    , rotateB
    , mousePointerB
    ) where

import Component.Behavior.Behavior
import Component.Behavior.EnclosedBehavior (encloseToBox, encloseByWrapAround)
import Component.Behavior.MousePointerBehavior (mousePositionCopier, mouseFollower, mousePointer)
import Component.Behavior.RotationalBehavior (rotate)
import Killable (die)

encloseToBoxB :: Behavior
encloseToBoxB = Behavior encloseToBox

encloseByWrapAroundB :: Behavior
encloseByWrapAroundB = Behavior encloseByWrapAround

noopB :: Behavior 
noopB = Behavior return

deadManWalkingB :: Behavior
deadManWalkingB = Behavior (return . die)

mousePositionCopierB :: Behavior 
mousePositionCopierB = Behavior mousePositionCopier

mouseFollowerB :: Behavior
mouseFollowerB = Behavior mouseFollower

rotateB :: Float -> Behavior
rotateB f = Behavior (rotate f)

mousePointerB :: Behavior
mousePointerB = Behavior mousePointer