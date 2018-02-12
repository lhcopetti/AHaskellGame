module Physics.Library.DebugDraw.DebugDraw
    where

import Control.Monad.Trans.Maybe (MaybeT)

import Physics.Library.Hipmunk.DebugDraw.DebugDraw as HMP
import GameObject.GameObjectTypes

mkDebugDraw :: Physics -> MaybeT IO Drawing
mkDebugDraw = HMP.mkDebugDraw