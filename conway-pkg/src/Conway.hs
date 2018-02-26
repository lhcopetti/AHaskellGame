{-# LANGUAGE NamedFieldPuns #-}
module Conway 
    ( newConwayWorld
    , setLive
    , setDead
    , cellAt
    , tick
    , BoardSize
    , ConwayWorld
    , Position
    ) where

import Data.List (intercalate, intersperse)
import Control.Monad (liftM)

import ConwayBoard
import ConwayCell

data ConwayWorld = ConwayWorld { world :: Board }

instance Show ConwayWorld where
    show ConwayWorld { world } = show world

newConwayWorld :: BoardSize -> Maybe ConwayWorld
newConwayWorld dimensions = liftM ConwayWorld (newBoard dimensions)

setLive :: Position -> ConwayWorld -> ConwayWorld
setLive pos ConwayWorld { world } = ConwayWorld (setLiveCell pos world)

setDead :: Position -> ConwayWorld -> ConwayWorld
setDead pos ConwayWorld { world } = ConwayWorld (setDeadCell pos world)

tick :: ConwayWorld -> ConwayWorld
tick ConwayWorld { world } = ConwayWorld { world = tickBoard world }

cellAt :: Position -> ConwayWorld -> Maybe ConwayCell
cellAt pos ConwayWorld { world } = atPosition pos world