module Math.MathFunctions
    ( signumWithoutZero
    ) where

signumWithoutZero :: (Num a, Eq a) => a -> a
signumWithoutZero x = let sign = signum x in
    case sign of 
        0 -> 1
        otherwise -> sign
