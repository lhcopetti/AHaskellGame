module ListOfLists 
    ( LL (..)
    ) where

import Data.List (intercalate)

newtype LL a    = LL [[a]]

instance Show a => Show (LL a) where
    show (LL b) = intercalate "\n" . map showColumns $ b
        where showColumns = unwords . map show

instance Foldable LL where
    foldr f value (LL x) = foldr f value (concat x)