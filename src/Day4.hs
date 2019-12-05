module Day4 where

import Control.Applicative (liftA2)
import Data.List (group)
import IntUtil (digits)

isDescending :: Ord a => [a] -> Bool
isDescending = all (uncurry (>=)) . (zip <*> tail)

hasAdjacent :: Eq a => [a] -> Bool
hasAdjacent = any (uncurry (==)) . (zip <*> tail)

hasGroupOf :: Eq a => Int -> [a] -> Bool
hasGroupOf n = any ((== n) . length) . group

isValidPassword :: Integral a => a -> Bool
isValidPassword = liftA2 (&&) isDescending hasAdjacent . digits

isValidPassword' :: Integral a => a -> Bool
isValidPassword' = liftA2 (&&) isDescending (hasGroupOf 2) . digits

solve1 :: Int -> Int -> Int
solve1 x y = length $ filter isValidPassword [x .. y]

solve2 :: Int -> Int -> Int
solve2 x y = length $ filter isValidPassword' [x .. y]
