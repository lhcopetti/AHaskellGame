module Component.Draw.FlaggedDrawing
    ( createSingleFlagDrawing
    ) where

import GameObject.GameObjectTypes (Drawing (..), DrawingFlag (..))

createSingleFlagDrawing :: Drawing -> DrawingFlag -> Drawing
createSingleFlagDrawing drw flag = FlaggedDrawing drw [flag]