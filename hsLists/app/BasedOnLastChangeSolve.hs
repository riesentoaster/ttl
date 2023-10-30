module BasedOnLastChangeSolve where

import Board
import Data.List (transpose)
import ManualSolve (solveAndFilterManual)

isBoardOKBasedOnLastChange :: (Int, Int, Board) -> Bool
isBoardOKBasedOnLastChange (x, y, b) =
  areRowCountsOK row
    && areRowCountsOK col
    && not (hasDuplicateRowsBasedOnRow b x)
    && not (hasDuplicateRowsBasedOnRow transposed y)
    && not (hasRowTriplets row)
    && not (hasRowTriplets col)
  where
    row = b !! x
    transposed = transpose b
    col = transposed !! y

hasDuplicateRowsBasedOnRow :: Board -> Int -> Bool
hasDuplicateRowsBasedOnRow b i = notElem E currentRow && length (filter (== currentRow) b) /= 1
  where
    currentRow = b !! i

trd :: (a, b, c) -> c
trd (_, _, c) = c

getValidExpansions :: Board -> [Board]
getValidExpansions =
  solveAndFilterManual
    . map trd
    . filter isBoardOKBasedOnLastChange
    . expandBasedOnLastChange

expandBasedOnLastChange :: Board -> [(Int, Int, Board)]
expandBasedOnLastChange m
  | null postRows = []
  | null postRow = []
  | otherwise = [(x, y, preRows ++ [preRow ++ c : tail postRow] ++ tail postRows) | c <- [X, O]]
  where
    (preRows, postRows) = break (elem E) m
    (preRow, postRow) = break (== E) $ head postRows
    x = length preRows
    y = length preRow

solveOKBoard :: Board -> [Board]
solveOKBoard b =
  if isBoardFull b
    then [b]
    else concatMap solveOKBoard (getValidExpansions b)

solveBasedOnLastChange :: Board -> [Board]
solveBasedOnLastChange m = if not (isBoardOK m) then [] else solveOKBoard m