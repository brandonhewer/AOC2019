module Day8 where

import Data.Char (digitToInt, intToDigit)
import Data.List (intercalate)
import Data.List.Split (chunksOf)

data MList a
  = Point a
  | List [MList a]
  deriving (Show)

instance Functor MList where
  fmap f (Point x) = Point (f x)
  fmap f (List xs) = List $ map (fmap f) xs

instance Foldable MList where
  foldr f z (Point x) = f x z
  foldr f z (List xs) = foldr (flip $ foldr f) z xs

countOccurs :: (Eq a, Foldable f) => a -> f a -> Int
countOccurs x =
  foldr
    (\y n ->
       if x == y
         then n + 1
         else n)
    0

liftML :: ([MList a] -> [[MList a]]) -> MList a -> MList a
liftML _ (Point x) = Point x
liftML f (List xs) = List $ map List (f xs)

foldML :: (MList a -> b -> b) -> b -> MList a -> b
foldML f z (Point x) = f (Point x) z
foldML f z (List xs) = foldr f z xs

toMList :: [a] -> MList a
toMList = List . map Point

fromMList :: MList a -> [a]
fromMList (Point x) = [x]
fromMList (List xs) = xs >>= fromMList

blocks :: [Int] -> [a] -> MList a
blocks = flip $ foldr (liftML . chunksOf) . toMList

zipWithML :: (a -> b -> c) -> MList a -> MList b -> MList c
zipWithML f (Point x) (Point y) = Point (f x y)
zipWithML _ (Point _) (List []) = List []
zipWithML f x@(Point _) (List (y:ys)) = zipWithML f x y
zipWithML f (List xs) (List ys) = List $ zipWith (zipWithML f) xs ys

zipFoldML :: (a -> a -> a) -> MList a -> MList a
zipFoldML _ x@(Point _) = x
zipFoldML _ (List []) = List []
zipFoldML f (List (x:xs)) = foldl (zipWithML f) x xs

compareFind :: (MList a -> b) -> (b -> b -> Bool) -> MList a -> MList a
compareFind _ _ x@(Point _) = x
compareFind _ _ x@(List []) = List []
compareFind f c (List (x:xs)) = fst $ foldr go (x, f x) xs
  where
    go y (w, m) =
      let n = f y
       in if c n m
            then (y, n)
            else (w, m)

solve1 :: [String] -> Int
solve1 xs =
  let layer = leastZeroes xs
   in countOccurs 1 layer * countOccurs 2 layer
  where
    leastZeroes =
      compareFind (countOccurs 0) (<) . blocks [6, 25] . (>>= map digitToInt)

solve2 :: [String] -> String
solve2 =
  intercalate "\n" .
  map (map toDigit) .
  fromMList2 . zipFoldML colour . blocks [6, 25] . (>>= map digitToInt)
  where
    colour 2 n = n
    colour n m = n
    toDigit 0 = ' '
    toDigit n = '*'
    fromMList2 (Point x) = [[x]]
    fromMList2 (List xs) = map fromMList xs
