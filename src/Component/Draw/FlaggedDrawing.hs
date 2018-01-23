module Component.Draw.FlaggedDrawing
    ( createSingleFlagDrawing
    ) where

import Component.Draw.DrawingData (Drawing (..), DrawingFlag (..))

createSingleFlagDrawing :: Drawing -> DrawingFlag -> Drawing
createSingleFlagDrawing drw flag = FlaggedDrawing drw [flag]