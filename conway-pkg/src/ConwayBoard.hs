module ConwayBoard where

import ConwayCell
import ListOfLists

type Board      = LL ConwayCell
type Position   = (Int, Int)
type BoardSize  = (Int, Int)

newBoard :: BoardSize -> Maybe Board
newBoard (width, height)
    | width <= 0 || height <= 0 = Nothing
    | otherwise                 = Just (LL board)
    where 
        board = replicate height (replicate width deadCell)

boardSize :: Board -> BoardSize
boardSize (LL xs) = (length (head xs), length xs)

setCellAt :: Position -> ConwayCell -> Board -> Board
setCellAt (x, y) cell (LL b) = LL (replaceAt y newColumn b)
    where
        newColumn = replaceAt x cell (b !! y)

setLiveCell :: Position -> Board -> Board
setLiveCell pos = setCellAt pos liveCell

setDeadCell :: Position -> Board -> Board
setDeadCell pos = setCellAt pos deadCell

isLive :: Position -> Board -> Maybe Bool
isLive pos b = undefined

atPosition :: Position -> Board -> Maybe ConwayCell
atPosition = llAt

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i v xs = take i xs ++ [v] ++ drop (i+1) xs

getNeighbours :: Position -> Board -> [ConwayCell]
getNeighbours pos board = undefined

tickBoard :: Board -> Board
tickBoard board = undefined