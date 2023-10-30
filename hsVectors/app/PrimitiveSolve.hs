module PrimitiveSolve where

import Board
import Data.Vector as V
-- import Data.Vector as UV
import Data.Vector.Unboxed as UV

expand :: Board -> [Board]
expand m
  | V.null postRows = []
  | UV.null postRow = []
  | otherwise =
      [ preRows
          V.++ V.singleton (preRow UV.++ UV.singleton c UV.++ UV.tail postRow)
          V.++ V.tail postRows
        | c <- [X, O]
      ]
  where
    (preRows, postRows) = V.break (UV.elem E) m
    (preRow, postRow) = UV.break (== E) $ V.head postRows

primitiveSolve :: Board -> [Board]
primitiveSolve b
  | not (isBoardOK b) = []
  | isBoardFull b = [b]
  | otherwise = Prelude.concatMap primitiveSolve $ expand b