module ConwayCell
    ( ConwayCell (..)
    , deadCell
    , liveCell
    )
    where

data ConwayCell = ConwayCell    { alive :: Bool
                                }

instance Show ConwayCell where
    show ConwayCell {alive = True}  = "[*]"
    show ConwayCell {alive = False} = "[ ]"


liveCell :: ConwayCell
liveCell = ConwayCell { alive = True }

deadCell :: ConwayCell
deadCell = ConwayCell { alive = False }