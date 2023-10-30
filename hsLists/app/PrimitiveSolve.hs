module PrimitiveSolve where

import Board

expand :: Board -> [Board]
expand m
  | null postRows = []
  | null postRow = []
  | otherwise = [preRows ++ [preRow ++ c : tail postRow] ++ tail postRows | c <- [X, O]]
  where
    (preRows, postRows) = break (elem E) m
    (preRow, postRow) = break (== E) $ head postRows

primitiveSolve :: Board -> [Board]
primitiveSolve b
  | not (isBoardOK b) = []
  | isBoardFull b = [b]
  | otherwise = concatMap primitiveSolve $ expand b