module Killable
    ( Killable
    , isAlive
    , die
    ) where

class Killable a where 
    isAlive :: a -> Bool
    die :: a -> a