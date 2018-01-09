module Killable
    ( Killable
    , isAlive
    , kill
    ) where

class Killable a where 
    isAlive :: a -> Bool
    kill :: a -> a