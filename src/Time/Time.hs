module Time.Time
    ( formatSeconds
    ) where

import Text.Printf

formatSeconds :: Integer -> String
formatSeconds t
    | t < 0 = "--:--:--"
    | otherwise = let
        sec = t `mod` 60
        min = t `div` 60 `mod` 60
        hour = div t 3600
        in
            printf "%02d:%02d:%02d" hour min sec