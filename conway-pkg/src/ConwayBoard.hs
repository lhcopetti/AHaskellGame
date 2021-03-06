module ConwayBoard where

import Control.Monad (guard, liftM, MonadPlus, mzero)
import Data.Maybe (catMaybes)

import ConwayCell
import ListOfLists

type Board      = LL ConwayCell
type Position   = (Int, Int)
type BoardSize  = (Int, Int)

newBoard :: (MonadPlus m) => BoardSize -> m Board
newBoard (width, height) = do
    guard (width > 0 && height > 0)
    let board = replicate height (replicate width deadCell)
    pure (LL board)

resetBoard :: Board -> Board
resetBoard board = let 
    allPos = positionsFor board
    in foldr setDeadCell board allPos

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

isLiveCell :: Position -> Board -> Maybe Bool
isLiveCell = (liftM isAlive .) . atPosition

toggleCell :: Position -> Board -> Board
toggleCell pos b = case atPosition pos b of
    Nothing -> b
    Just c  -> setCellAt pos (toggle c) b


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
cellLiveNeighbours pos = filter isAlive . cellNeighbours pos


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
        [(x, y) | y <- [0..height-1], x <- [0..width-1]]

positionCellMapping :: Board -> [(Position, ConwayCell)]
positionCellMapping b = zip (positionsFor b) (allCells b)

tickBoard :: Board -> Board
tickBoard b = snd $ foldr acc (b, b) (positionCellMapping b)
    where
        acc (pos, cell) (static, change) = (static, setCellAt pos (stepCell neighbourCount cell) change)
            where
                neighbourCount = countLiveNeighbours pos static