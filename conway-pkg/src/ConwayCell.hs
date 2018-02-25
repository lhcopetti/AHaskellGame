{-# LANGUAGE NamedFieldPuns #-}
module ConwayCell
    ( ConwayCell (..)
    , deadCell
    , liveCell
    , isLiveCell
    , stepCell
    , stepDeadCell
    , stepLiveCell
    )
    where

data ConwayCell = ConwayCell    { alive :: Bool
                                } deriving Eq

instance Show ConwayCell where
    show ConwayCell {alive = True}  = "[*]"
    show ConwayCell {alive = False} = "[ ]"


liveCell :: ConwayCell
liveCell = ConwayCell { alive = True }

deadCell :: ConwayCell
deadCell = ConwayCell { alive = False }

isLiveCell :: ConwayCell -> Bool
isLiveCell ConwayCell { alive } = alive

stepCell :: Int -> ConwayCell -> ConwayCell
stepCell = undefined

stepDeadCell :: Int -> ConwayCell -> ConwayCell
stepDeadCell neighbourCount cell
    | neighbourCount == 3 = cell { alive = True } -- Reproduction
    | otherwise = cell

stepLiveCell :: Int -> ConwayCell -> ConwayCell
stepLiveCell neighbourCount cell
    | neighbourCount < 2 = cell { alive = False } -- Underpopulation
    | neighbourCount > 3 = cell { alive = False } -- Overpopulation
    | otherwise = cell -- Lives on to the next generation