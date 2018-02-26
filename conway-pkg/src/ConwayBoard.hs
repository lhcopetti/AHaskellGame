module ConwayBoard where

import Control.Monad (guard)
import Data.Maybe (catMaybes)

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
setCellAt = llReplace

setLiveCell :: Position -> Board -> Board
setLiveCell pos = setCellAt pos liveCell

setLiveCells :: [Position] -> Board -> Board
setLiveCells pos board = foldr ($) board f
    where
        f = map setLiveCell pos

setDeadCell :: Position -> Board -> Board
setDeadCell pos = setCellAt pos deadCell

isLive :: Position -> Board -> Maybe Bool
isLive pos b = undefined

atPosition :: Position -> Board -> Maybe ConwayCell
atPosition pos@(x, y) b = do
    guard (x >= 0 && y >= 0)
    llAt pos b

getNeighbours :: Position -> Board -> [ConwayCell]
getNeighbours pos board = undefined

allCells :: Board -> [ConwayCell]
allCells = llFlat

countLiveNeighbours :: Position -> Board -> Int
countLiveNeighbours pos = length . cellLiveNeighbours pos

cellLiveNeighbours :: Position -> Board -> [ConwayCell]
cellLiveNeighbours pos = filter isLiveCell . cellNeighbours pos


cellNeighbours :: Position -> Board -> [ConwayCell]
cellNeighbours pos b = catMaybes allCells
    where
        positions = neighbours pos
        allCells = map atPosition positions <*> [b]


neighbours :: Position -> [Position]
neighbours (cx, cy) = do
    x <- [0, -1, 1]
    y <- [0, -1, 1]
    guard (x /= 0 || y /= 0)
    return (cx + x, cy + y)

positionsFor :: Board -> [Position]
positionsFor b = 
    let (width, height) = boardSize b
    in
        [(x, y) | x <- [0..width-1], y <- [0..height-1]]

positionCellMapping :: Board -> [(Position, ConwayCell)]
positionCellMapping b = zip (positionsFor b) (allCells b)

tickBoard :: Board -> Board
tickBoard b = snd $ foldr acc (b, b) (positionCellMapping b)
    where
        acc (pos, cell) (static, change) = (static, setCellAt pos (stepCell neighbourCount cell) change)
            where
                neighbourCount = countLiveNeighbours pos static