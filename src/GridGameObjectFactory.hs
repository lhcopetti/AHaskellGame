module GridGameObjectFactory
    ( GridSize
    , createGridObjects
    ) where

import Control.Monad.Trans.Maybe (MaybeT)
import GameObject.GameObjectTypes

type GridSize = (Int, Int)
type Position = (Int, Int)


createGridObjects   :: GridSize
                    -> (Position -> MaybeT IO GameObject) 
                    -> MaybeT IO [GameObject]
createGridObjects (gw, gh) f = mapM f pos
    where
        pos = [(x, y) | y <- [0..gh-1], x <- [0..gw-1]]