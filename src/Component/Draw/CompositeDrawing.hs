module Component.Draw.CompositeDrawing
    ( createComposite
    ) where

import Control.Monad.Trans.Maybe (MaybeT)
import GameObject.GameObjectTypes (Drawing (..))

createComposite :: [Drawing] -> MaybeT IO Drawing
createComposite drw = return $ CompositeDrawing drw