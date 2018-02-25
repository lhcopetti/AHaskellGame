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

type BoardSize  = (Int, Int)

data ConwayWorld = ConwayWorld { world :: Board }

instance Show ConwayWorld where
    show ConwayWorld { world } = intercalate "\n" . map showColumns $ world
        where showColumns = unwords . map show

newConwayWorld :: BoardSize -> ConwayWorld
newConwayWorld dimensions = ConwayWorld { world = newBoard dimensions }

newBoard :: BoardSize -> Board
newBoard (width, height) = replicate height (replicate width deadCell)

setLive :: Position -> ConwayWorld -> ConwayWorld
setLive pos ConwayWorld { world } = ConwayWorld (setLiveCell pos world)

tick :: ConwayWorld -> ConwayWorld
tick ConwayWorld { world } = ConwayWorld { world = tickBoard world }