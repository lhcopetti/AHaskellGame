module Component.Draw.ZOrderable
    ( ZOrderable
    , getZ
    , setZ
    , orderByZ
    ) where

import Data.Function (on)
import Data.List (sortBy)

class ZOrderable a where
    getZ :: a -> Float
    setZ :: Float -> a -> a


orderByZ :: ZOrderable a => [a] -> [a]
orderByZ = sortBy (compare `on` getZ)