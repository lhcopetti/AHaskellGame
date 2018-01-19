module Component.Draw.CompositeDrawing
    ( createComposite
    ) where

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import Component.Draw.DrawingData
        
createComposite :: [Drawing] -> MaybeT IO Drawing
createComposite drw = return $ CompositeDrawing drw