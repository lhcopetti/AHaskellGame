module Container.TupleHelper
    ( mapTuple
    ) where


mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)