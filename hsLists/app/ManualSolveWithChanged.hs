module ManualSolveWithChanged where

import Board
import Data.List (transpose)

data Changed a = Changed a | Same a

instance (Eq a) => Eq (Changed a) where
  a == b = extract a == extract b

isChanged :: Changed a -> Bool
isChanged (Changed _) = True
isChanged _ = False

mapAndKeepChanged :: (a -> a) -> Changed a -> Changed a
mapAndKeepChanged f (Changed a) = Changed $ f a
mapAndKeepChanged f (Same a) = Same $ f a

setChanged :: (a -> a) -> Changed a -> Changed a
setChanged f (Changed a) = Changed $ f a
setChanged f (Same a) = Changed $ f a

extract :: Changed a -> a
extract (Changed a) = a
extract (Same a) = a

sameAs :: Changed a -> b -> Changed b
sameAs (Changed _) e = Changed e
sameAs _ e = Same e

wrapChanged :: Bool -> a -> Changed a
wrapChanged b e = if b then Changed e else Same e

fix :: (Eq a) => (Changed a -> Changed a) -> Changed a -> Changed a
fix f e = if res == e then e else fix f res
  where
    res = f e

other :: Field -> Field
other O = X
other X = O
other E = E

fixTriplets :: Changed Row -> Changed Row
fixTriplets b = f (sameAs b []) (extract b)
  where
    f acc [] = acc
    f acc e@[_] = mapAndKeepChanged (++ e) acc
    f acc e@[_, _] = mapAndKeepChanged (++ e) acc
    f acc (e1 : e2 : e3 : es)
      | e1 == E && e2 == e3 = f (setChanged (++ [other e2]) acc) (e2 : e3 : es)
      | e2 == E && e1 == e3 = f (setChanged (++ [e1, other e1]) acc) (e3 : es)
      | e3 == E && e1 == e2 = f (setChanged (++ [e1, e2]) acc) (other e2 : es)
      | otherwise = f (mapAndKeepChanged (++ [e1]) acc) (e2 : e3 : es)

fixCountForFullRow :: Changed Row -> Changed Row
fixCountForFullRow r
  | countElem X extracted >= len `div` 2 = Changed (replace E O extracted)
  | countElem O extracted >= len `div` 2 = Changed (replace E X extracted)
  | otherwise = r
  where
    extracted = extract r
    len = length extracted

replace :: (Eq a) => a -> a -> [a] -> [a]
replace find repl = map (\cur -> if cur == find then repl else cur)

solveAndFilterManual :: [Board] -> [Board]
solveAndFilterManual = map extract . filter (\b -> not (isChanged b) || (isBoardOK . extract) b) . map solveManual

-- solveAndFilterManual = map fst . filter filterer . map mapper
--   where
--     filterer (b, change) = change || isBoardOK b
--     mapper b = (solved, solved == b)
--       where
--         solved = solveManual b

transposeChanged :: [Changed Row] -> [Changed Row]
transposeChanged rs = (map (wrapChanged (any isChanged rs)) . transpose . map extract) rs

solveManual :: Board -> Changed Board
solveManual bs = wrapChanged (any isChanged res) (map extract res)
  where
    res = (transposeChanged . map f . transposeChanged . map (f . Same)) bs
    f =
      fix
        fixTriplets
        . fixCountForFullRow