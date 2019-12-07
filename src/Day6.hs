{-# LANGUAGE TupleSections #-}

module Day6 where

import Data.List (delete)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

data OrbitMap a =
  OrbitMap
    { edges :: Map.Map a [a]
    , parents :: Map.Map a a
    }

sumDirect :: Ord a => a -> Map.Map a [a] -> Int
sumDirect s m =
  case Map.lookup s m of
    Nothing -> 0
    Just xs -> foldr (\x acc -> 1 + acc + sumDirect x m) 0 xs

sumIndirect :: Ord a => a -> Map.Map a [a] -> Int
sumIndirect = go id
  where
    go f s m =
      case Map.lookup s m of
        Nothing -> 0
        Just xs -> foldr (\x acc -> f $ acc + go (f . (1 +)) x m) 0 xs

makeOrbitMap :: (Ord a, Foldable t) => [a] -> t [a] -> OrbitMap [a]
makeOrbitMap d ss =
  let (es, ps) = foldr addEdge ([], []) ss
   in OrbitMap (Map.fromListWith (++) es) (Map.fromList ps)
  where
    addEdge s (es, ps) =
      case splitOn d s of
        [] -> (es, ps)
        (v:vs) -> ((v, vs) : es, ps ++ map (, v) vs)

removeEdge :: Ord a => a -> a -> OrbitMap a -> OrbitMap a
removeEdge s e (OrbitMap es ps) =
  OrbitMap (Map.adjust (delete e) s es) (Map.delete e ps)

liftMaybe2 :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
liftMaybe2 f Nothing Nothing = Nothing
liftMaybe2 f x Nothing = x
liftMaybe2 f Nothing y = y
liftMaybe2 f (Just x) (Just y) = Just $ f x y

hop :: Ord a => a -> OrbitMap a -> Maybe [(a, OrbitMap a)]
hop s m@(OrbitMap es ps) =
  let p = Map.lookup s ps >>= \x -> Just (x, removeEdge x s m)
      c = map (\x -> (x, removeEdge s x m)) <$> Map.lookup s es
   in liftMaybe2 (++) (pure <$> p) c

shortestPath :: Ord a => a -> a -> OrbitMap a -> Maybe Int
shortestPath s e m@(OrbitMap es ps)
  | s == e = Just 0
  | otherwise =
    hop s m >>=
    foldr (liftMaybe2 min) Nothing .
    map (\(u, n) -> (1 +) <$> shortestPath u e n)

solve1 :: [String] -> Int
solve1 xs =
  let OrbitMap es _ = makeOrbitMap ")" xs
   in sumDirect "COM" es + sumIndirect "COM" es

solve2 :: [String] -> Maybe Int
solve2 xs =
  let m@(OrbitMap es ps) = makeOrbitMap ")" xs
      Just yp = Map.lookup "YOU" ps
      Just sp = Map.lookup "SAN" ps
   in shortestPath yp sp m
