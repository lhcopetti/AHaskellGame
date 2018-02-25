{-# LANGUAGE NamedFieldPuns #-}
module Conway 
    ( newConwayWorld
    , setLive
    , BoardSize
    , ConwayWorld
    , Position
    ) where

import Data.List (intercalate, intersperse)

import ConwayBoard
import ConwayCell

data ConwayWorld = ConwayWorld { world :: Board }

instance Show ConwayWorld where
    show ConwayWorld { world } = show world

newConwayWorld :: BoardSize -> ConwayWorld
newConwayWorld dimensions = ConwayWorld { world = newBoard dimensions }

setLive :: Position -> ConwayWorld -> ConwayWorld
setLive pos ConwayWorld { world } = ConwayWorld (setLiveCell pos world)

tick :: ConwayWorld -> ConwayWorld
tick ConwayWorld { world } = ConwayWorld { world = tickBoard world }