module Component.Draw.NamedDrawing
    ( createNamedDrawing
    ) where

import GameObject.GameObjectTypes (Drawing (NamedDrawing))

createNamedDrawing :: String -> Drawing -> Drawing
createNamedDrawing = NamedDrawing