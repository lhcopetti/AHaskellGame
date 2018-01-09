module Killable
    ( Killable
    , isAlive
    , die
    , destroyResource
    ) where

class Killable a where 
    isAlive :: a -> Bool
    die :: a -> a
    destroyResource :: a -> IO ()