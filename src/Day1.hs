module Day1 where

fuelCost :: Int -> Int
fuelCost m = (m `div` 3) - 2

solve :: [String] -> Int
solve = sum . map (fuelCost . read)
