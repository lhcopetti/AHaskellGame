module Component.Draw.CompositeDrawing
    ( createComposite
    ) where

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Plus (liftM, mreturn)
import GameObject.GameObjectTypes (Drawing (..))

import qualified Data.List.NonEmpty as LNE

createComposite :: [Drawing] -> MaybeT IO Drawing
createComposite drw = liftM CompositeDrawing (mreturn LNE.nonEmpty drw)