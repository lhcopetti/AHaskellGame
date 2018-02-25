module ListOfLists 
    ( LL (..)
    , llAt
    ) where

import Data.List (intercalate)

newtype LL a    = LL [[a]]
        deriving (Eq)

instance Show a => Show (LL a) where
    show (LL b) = intercalate "\n" . map showColumns $ b
        where showColumns = unwords . map show

instance Foldable LL where
    foldr f value (LL x) = foldr f value (concat x)

llAt :: (Int, Int) -> LL a -> Maybe a
llAt (x, y) (LL ll) = at' y ll >>= at' x

-- | Implementation oriented by: [Avoiding partial functions](https://wiki.haskell.org/Avoiding_partial_functions)
at' :: Int -> [a] -> Maybe a
at' i xs = case drop i xs of
    x:_ -> Just x
    []  -> Nothing