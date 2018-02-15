module NativeResource
    ( NativeResource
    , free
    ) where

class NativeResource a where
    free :: a -> IO ()