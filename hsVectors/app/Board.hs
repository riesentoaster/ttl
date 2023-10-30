{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Board where

import Data.List (transpose)
import Data.Maybe (isNothing)
import Data.Vector as V
-- import Data.Vector as UV
import Data.Vector.Unboxed as UV
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Word (Word8)

data Field = E | X | O deriving (Read, Show, Eq)

fieldToWord8 :: Field -> Word8
fieldToWord8 E = 0
fieldToWord8 X = 1
fieldToWord8 O = 2

word8ToField :: Word8 -> Field
word8ToField 0 = E
word8ToField 1 = X
word8ToField 2 = O

derivingUnbox
  "Field"
  [t|Field -> Word8|]
  [|fieldToWord8|]
  [|word8ToField|]

type Row = UV.Vector Field

type Board = V.Vector Row

areRowCountsOK :: Row -> Bool
areRowCountsOK r = res
  where
    res = countElem O r <= maxValue && countElem X r <= maxValue
    countElem e = UV.foldr (\cur acc -> if cur == e then acc + 1 else acc) 0
    -- res = oCount <= maxValue && xCount <= maxValue
    -- (oCount, xCount) = UV.foldr f (0, 0) r
    -- f E acc = acc
    -- f O (l, r) = (1 + l, r)
    -- f X (l, r) = (l, 1 + r)
    maxValue = UV.length r `div` 2

hasRowTriplets :: Row -> Bool
hasRowTriplets r
  | UV.length r < 3 = False
  | otherwise =
      r1 /= E
        && r1 == r2
        && r1 == r3
        || hasRowTriplets (UV.tail r)
  where
    r1 = r UV.! 0
    r2 = r UV.! 1
    r3 = r UV.! 2

isBoardDone :: Board -> Bool
isBoardDone b = isBoardFull b && isBoardOK b

tr :: Board -> Board
tr = V.map UV.fromList . V.fromList . transpose . V.toList . V.map UV.toList

isBoardOK :: Board -> Bool
isBoardOK rs = isBoardOKBasedOnRowsOnly rs && isBoardOKBasedOnRowsOnly (tr rs)

isBoardOKBasedOnRowsOnly :: Board -> Bool
isBoardOKBasedOnRowsOnly rs =
  V.all areRowCountsOK rs
    && not (V.any hasRowTriplets rs)
    && not (hasDuplicateRows rs)

isBoardFull :: Board -> Bool
isBoardFull = V.all (UV.notElem E)

hasDuplicateRows :: Board -> Bool
hasDuplicateRows rs = V.ifoldr f False filtered
  where
    f i cur acc = acc || V.elem cur (extracted i)
    extracted i = snd (V.splitAt (i + 1) filtered)
    filtered = V.filter (UV.notElem E) rs

easy :: Board
easy = V.map UV.fromList . V.fromList $ [[X, O], [O, E]]

medium :: Board
medium = V.map UV.fromList . V.fromList $ [[X, O, E, X], [O, X, O, X], [X, O, X, O], [O, X, X, O]]

empty :: Int -> Int -> Board
empty x y = V.replicate x (UV.replicate y E)