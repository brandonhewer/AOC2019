module Main where

import qualified Day1 as Day1
import System.Environment

solve :: Int -> [String] -> String
solve 1 = show . Day1.solve
solve n = const "Solution does not yet exist"

main :: IO ()
main = do
  args <- getArgs
  input <- lines <$> readFile (args !! 1)
  putStrLn $ solve (read $ args !! 0) input
