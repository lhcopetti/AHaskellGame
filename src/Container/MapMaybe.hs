module Container.MapMaybe
    ( insertWithMaybe
    ) where


import qualified Data.Map as M

insertWithMaybe :: Ord k => (a -> a -> a) -> k -> Maybe a -> M.Map k a -> M.Map k a
insertWithMaybe f key mValue mmap = case mValue of
    Nothing -> mmap
    Just v  -> M.insertWith f key v mmap