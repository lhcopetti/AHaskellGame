module Synchronizable
    ( SynchronizableType
    , Synchronizable
    , synchronize
    ) where


type SynchronizableType a = a -> IO ()

class Synchronizable a where
    synchronize :: SynchronizableType a