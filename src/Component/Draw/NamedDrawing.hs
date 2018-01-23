module Component.Draw.NamedDrawing
    ( createNamedDrawing
    ) where

import Component.Draw.DrawingData (Drawing (NamedDrawing))

createNamedDrawing :: String -> Drawing -> Drawing
createNamedDrawing = NamedDrawing