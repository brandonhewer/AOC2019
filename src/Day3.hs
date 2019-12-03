{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Day3 where

import Control.Applicative (liftA2)
import Control.Monad.State.Strict
import Data.List (minimumBy)
import Data.List.Split (splitOn)

data Line a
  = ConstX a a a
  | ConstY a a a

type Path a = [Line a]

type Point a = (a, a)

moveX :: Num a => a -> Point a -> Point a
moveX dx (x, y) = (x + dx, y)

moveY :: Num a => a -> Point a -> Point a
moveY = fmap . (+)

makeLine :: (Num a, Read a, MonadState (Point a) m) => String -> m (Line a)
makeLine (d:sn) =
  let n = read sn
   in do (x, y) <- get
         case d of
           'R' -> modify (moveY n) >> return (ConstX x y (y + n))
           'U' -> modify (moveX n) >> return (ConstY y x (x + n))
           'L' -> modify (moveY (-n)) >> return (ConstX x y (y - n))
           'D' -> modify (moveX (-n)) >> return (ConstY y x (x - n))

between :: Ord a => a -> a -> a -> Bool
between a b x
  | a <= b = x >= a && x <= b
  | otherwise = x >= b && x <= a

intersections :: (Enum a, Eq a, Ord a) => Line a -> Line a -> [Point a]
intersections (ConstX x1 y1 y2) (ConstX x2 y3 y4)
  | x1 == x2 = map (x1, ) [(max y1 y3) .. (min y2 y4)]
  | otherwise = []
intersections (ConstY y1 x1 x2) (ConstY y2 x3 x4)
  | y1 == y2 = map (, y1) [(max x1 x3) .. (min x2 x4)]
  | otherwise = []
intersections (ConstX x3 y1 y2) (ConstY y3 x1 x2)
  | between x1 x2 x3 && between y1 y2 y3 = [(x3, y3)]
  | otherwise = []
intersections (ConstY y3 x1 x2) (ConstX x3 y1 y2)
  | between x1 x2 x3 && between y1 y2 y3 = [(x3, y3)]
  | otherwise = []

manhattan :: Num a => Point a -> Point a -> a
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

closest :: Ord a => (Point b -> a) -> [Point b] -> Point b
closest f = minimumBy (\x y -> compare (f x) (f y))

intersects :: (Applicative f, Enum a, Ord a) => [f (Line a)] -> [f [Point a]]
intersects = zipWith (liftA2 intersections) <*> tail

makeLines :: (Functor f, Num a, Read a) => f String -> f [Line a]
makeLines = fmap (flip evalState (0, 0) . traverse makeLine . splitOn ",")

solve1 :: [String] -> String
solve1 =
  show .
  manhattan (0, 0) .
  closest (manhattan (0, 0)) . join . join . intersects . makeLines
