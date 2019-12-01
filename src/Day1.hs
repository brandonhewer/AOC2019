module Day1 where

fuelCost :: Int -> Int
fuelCost m = (m `div` 3) - 2

iterFuelCost :: Int -> Int
iterFuelCost = sum . takeWhile (>= 0) . tail . iterate fuelCost

solve1 :: [String] -> Int
solve1 = sum . map (fuelCost . read)

solve2 :: [String] -> Int
solve2 = sum . map (iterFuelCost . read)
