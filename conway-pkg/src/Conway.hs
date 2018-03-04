{-# LANGUAGE NamedFieldPuns #-}
module Conway 
    ( newConwayWorld
    , resetConwayWorld
    , setLive
    , setLives
    , setDead
    , cellAt
    , tick
    , isLive
    , BoardSize
    , ConwayWorld
    , Position
    ) where

import Data.List (intercalate, intersperse)
import Control.Monad (liftM, MonadPlus)
import Data.Maybe (fromMaybe)

import ConwayBoard
import ConwayCell

data ConwayWorld = ConwayWorld { world :: Board }

instance Show ConwayWorld where
    show ConwayWorld { world } = show world

newConwayWorld :: (MonadPlus m) => BoardSize -> m ConwayWorld
newConwayWorld dimensions = liftM ConwayWorld (newBoard dimensions)

resetConwayWorld :: ConwayWorld -> ConwayWorld
resetConwayWorld ConwayWorld { world } = ConwayWorld { world = reset world }

setLive :: Position -> ConwayWorld -> ConwayWorld
setLive pos ConwayWorld { world } = ConwayWorld (setLiveCell pos world)

setLives :: [Position] -> ConwayWorld -> ConwayWorld
setLives pos ConwayWorld { world } = ConwayWorld (setLiveCells pos world)

setDead :: Position -> ConwayWorld -> ConwayWorld
setDead pos ConwayWorld { world } = ConwayWorld (setDeadCell pos world)

tick :: ConwayWorld -> ConwayWorld
tick ConwayWorld { world } = ConwayWorld { world = tickBoard world }

cellAt :: Position -> ConwayWorld -> Maybe ConwayCell
cellAt pos ConwayWorld { world } = atPosition pos world

isLive :: Position -> ConwayWorld -> Bool
isLive pos ConwayWorld { world } = fromMaybe False (isLiveCell pos world)