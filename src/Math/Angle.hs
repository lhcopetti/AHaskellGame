module Math.Angle
    ( toDegree
    ) where


toDegree :: (Floating a) => a -> a
toDegree radians = radians * 180 / pi