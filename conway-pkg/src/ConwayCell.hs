{-# LANGUAGE NamedFieldPuns #-}
module ConwayCell
    ( ConwayCell (..)
    , deadCell
    , liveCell
    , isLiveCell
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