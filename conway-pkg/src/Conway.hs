{-# LANGUAGE NamedFieldPuns #-}
module Conway 
    ( newConwayWorld
    , reset
    , setLive
    , setLives
    , setDead
    , cellAt
    , tick
    , isLive
    , toggleAt
    , BoardSize
    , ConwayWorld
    , Position
    ) where

import Control.Monad (liftM, MonadPlus)
import Data.Maybe (fromMaybe)

import ConwayBoard
import ConwayCell

data ConwayWorld = ConwayWorld { world :: Board }

instance Show ConwayWorld where
    show ConwayWorld { world } = show world

newConwayWorld :: (MonadPlus m) => BoardSize -> m ConwayWorld
newConwayWorld dimensions = liftM ConwayWorld (newBoard dimensions)

reset :: ConwayWorld -> ConwayWorld
reset = mapOnBoard resetBoard

setLive :: Position -> ConwayWorld -> ConwayWorld
setLive = mapOnBoard . setLiveCell

setLives :: [Position] -> ConwayWorld -> ConwayWorld
setLives = mapOnBoard . setLiveCells

toggleAt :: Position -> ConwayWorld -> ConwayWorld
toggleAt = mapOnBoard . toggleCell

setDead :: Position -> ConwayWorld -> ConwayWorld
setDead = mapOnBoard . setDeadCell

tick :: ConwayWorld -> ConwayWorld
tick = mapOnBoard tickBoard

cellAt :: Position -> ConwayWorld -> Maybe ConwayCell
cellAt = onBoard . atPosition

isLive :: Position -> ConwayWorld -> Bool
isLive pos = fromMaybe False . onBoard (isLiveCell pos)

mapOnBoard :: (Board -> Board) -> ConwayWorld -> ConwayWorld
mapOnBoard f ConwayWorld { world } = ConwayWorld { world = f world }

onBoard :: (Board -> a) -> ConwayWorld -> a
onBoard f ConwayWorld { world } = f world