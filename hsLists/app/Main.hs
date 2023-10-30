-- import BasedOnLastChangeSolve (solveBasedOnLastChange)

import BasedOnLastChangeSolve (solveBasedOnLastChange)
import Board (empty)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import PrimitiveSolve (primitiveSolve)
import System.Environment (getArgs)

parseArgs :: [String] -> Maybe (Int, Int)
parseArgs args
  | length parsed < 2 = Nothing
  | otherwise = Just (head parsed, parsed !! 1)
  where
    parsed = map read args

time :: (Show a) => String -> a -> IO ()
time title a = do
  start <- getCurrentTime
  print a
  end <- getCurrentTime
  print ("time for " ++ title ++ ": " ++ show (diffUTCTime end start))

main :: IO ()
main = do
  args <- getArgs
  let (x, y) = fromMaybe (6, 6) (parseArgs args)
  time "simple" $ length $ primitiveSolve $ empty x y

-- time "fancy" $ length $ solveBasedOnLastChange $ empty x y