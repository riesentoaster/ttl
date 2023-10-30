module ManualSolve where

import Board
import Data.List (transpose)

-- both :: (a -> b) -> (a, a) -> (b, b)
-- both f (l, r) = (f l, f r)

-- countElements :: Row -> (Int, Int)
-- countElements r = (countElem X r, countElem O r)

fix :: (Eq a) => (a -> a) -> a -> a
fix f e = if res == e then e else fix f res
  where
    res = f e

other :: Field -> Field
other O = X
other X = O
other E = E

fixTriplets :: Row -> Row
fixTriplets = f []
  where
    f acc [] = reverse acc
    f acc [e] = reverse (e : acc)
    f acc [e1, e2] = reverse (e2 : e1 : acc)
    f acc (e1 : e2 : e3 : es)
      | e1 == E && e2 == e3 = f (other e2 : acc) (e2 : e3 : es)
      | e2 == E && e1 == e3 = f (other e1 : e1 : acc) (e3 : es)
      | e3 == E && e1 == e2 = f (e2 : e1 : acc) (other e2 : es)
      | otherwise = f (e1 : acc) (e2 : e3 : es)

fixCountForFullRow :: Row -> Row
fixCountForFullRow r
  | countElem X r >= maxCount = replaceSingle E O r
  | countElem O r >= maxCount = replaceSingle E X r
  | otherwise = r
  where
    maxCount = length r `div` 2

replaceSingle :: (Eq a) => a -> a -> [a] -> [a]
replaceSingle find repl = map (\cur -> if cur == find then repl else cur)

solveAndFilterManual :: [Board] -> [Board]
solveAndFilterManual =
  map snd
    . filter (\(e, te) -> isBoardOKBasedOnRowsOnly e && isBoardOKBasedOnRowsOnly te)
    . map ((\e -> (e, transpose e)) . map f . transpose . map f)
  where
    f =
      fixCountForFullRow
        . fixTriplets