module Component.Draw.NamedDrawing
    ( createNamedDrawing
    , createNamedDrawingM
    ) where

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad (liftM)

import GameObject.GameObjectTypes (Drawing (NamedDrawing))

createNamedDrawing :: String -> Drawing -> Drawing
createNamedDrawing = NamedDrawing

createNamedDrawingM :: String -> MaybeT IO Drawing -> MaybeT IO Drawing
createNamedDrawingM name = liftM (createNamedDrawing name)