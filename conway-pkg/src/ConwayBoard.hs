module ConwayBoard where

import ConwayCell

type Board      = [ [ConwayCell] ]
type Position   = (Int, Int)

setCellAt :: Position -> ConwayCell -> Board -> Board
setCellAt (x, y) cell board = replaceAt y newColumn board
    where
        newColumn = replaceAt x cell (board !! y)

setLiveCell :: Position -> Board -> Board
setLiveCell pos = setCellAt pos liveCell

setDeadCell :: Position -> Board -> Board
setDeadCell pos = setCellAt pos deadCell

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i v xs = take i xs ++ [v] ++ drop (i+1) xs

getNeighbours :: Position -> Board -> [ConwayCell]
getNeighbours pos board = undefined

tickBoard :: Board -> Board
tickBoard board = undefined