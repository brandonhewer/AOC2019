module Main where

import qualified Day1 as Day1
import qualified Day2 as Day2
import qualified Day3 as Day3
import System.Environment

solve :: Int -> Int -> [String] -> String
solve 1 1 = show . Day1.solve1
solve 1 2 = show . Day1.solve2
solve 2 1 = Day2.solve1
solve 2 2 = Day2.solve2
solve 3 1 = Day3.solve1
solve _ _ = const "Solution does not yet exist"

main :: IO ()
main = do
  args <- getArgs
  input <- lines <$> readFile (args !! 2)
  putStrLn $ solve (read $ args !! 0) (read $ args !! 1) input
