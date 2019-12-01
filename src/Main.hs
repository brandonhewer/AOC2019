module Main where

import qualified Day1 as Day1
import System.Environment

solve :: Int -> Int -> [String] -> String
solve 1 1 = show . Day1.solve1
solve 1 2 = show . Day1.solve2
solve _ _ = const "Solution does not yet exist"

main :: IO ()
main = do
  args <- getArgs
  input <- lines <$> readFile (args !! 2)
  putStrLn $ solve (read $ args !! 0) (read $ args !! 1) input
