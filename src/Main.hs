module Main where

import qualified Day1 as Day1
import qualified Day2 as Day2
import qualified Day3 as Day3
import qualified Day4 as Day4
import qualified Day5 as Day5
import qualified Day6 as Day6
import qualified Day7 as Day7
import System.Environment

solve :: Int -> Int -> [String] -> String
solve 1 1 = show . Day1.solve1
solve 1 2 = show . Day1.solve2
solve 2 1 = Day2.solve1
solve 2 2 = Day2.solve2
solve 3 1 = Day3.solve1
solve 3 2 = Day3.solve2
solve 4 1 = const $ show $ Day4.solve1 240298 784956
solve 4 2 = const $ show $ Day4.solve2 240298 784956
solve 5 1 = Day5.solve 1
solve 5 2 = Day5.solve 5
solve 6 1 = show . Day6.solve1
solve 6 2 = show . Day6.solve2
solve 7 1 = Day7.solve1
solve 7 2 = Day7.solve2
solve _ _ = const "Solution does not yet exist"

solveF :: Int -> Int -> String -> IO String
solveF d p f = do
  input <- lines <$> readFile f
  return $ solve d p input

main :: IO ()
main = do
  args <- getArgs
  if length args >= 2
    then do
      let day = read $ args !! 0
          part = read $ args !! 1
      if length args >= 3
        then solveF day part (args !! 2) >>= putStrLn
        else putStrLn $ solve day part []
    else putStrLn "No Day/Part given as arguments"
