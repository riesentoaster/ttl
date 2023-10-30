module Board where

import Data.List (transpose)

data Field = E | X | O deriving (Show, Eq, Read)

type Row = [Field]

type Board = [[Field]]

countElem :: (Eq a) => a -> [a] -> Int
countElem e = length . filter (== e)

areRowCountsOK :: Row -> Bool
areRowCountsOK fs =
  countElem X fs <= len
    && countElem O fs <= len
  where
    len = length fs `div` 2

hasRowTriplets :: Row -> Bool
hasRowTriplets (r1 : r2 : r3 : rs) =
  r1 /= E
    && r1 == r2
    && r1 == r3
    || hasRowTriplets (r2 : r3 : rs)
hasRowTriplets _ = False

isBoardDone :: Board -> Bool
isBoardDone b = isBoardFull b && isBoardOK b

isBoardOK :: Board -> Bool
isBoardOK rs = isBoardOKBasedOnRowsOnly rs && isBoardOKBasedOnRowsOnly (transpose rs)

isBoardOKBasedOnRowsOnly :: Board -> Bool
isBoardOKBasedOnRowsOnly rs =
  not (any hasRowTriplets rs)
    && not (hasDuplicateRows rs)
    && all areRowCountsOK rs

isBoardFull :: Board -> Bool
isBoardFull = all (notElem E)

hasDuplicateRows :: Board -> Bool
hasDuplicateRows [] = False
hasDuplicateRows (r : rs) = notElem E r && elem r rs || hasDuplicateRows rs

easy :: Board
easy = [[X, O], [O, E]]

medium :: Board
medium = [[X, O, E, X], [O, X, O, X], [X, O, X, O], [O, X, X, O]]

empty :: Int -> Int -> Board
empty x y = replicate x (replicate y E)