module ZipSequence where

import Data.Sequence

data ZipSeq a =
  Pos (Seq a) (Int, a) (Seq a)
  deriving (Show)

instance Functor ZipSeq where
  fmap f (Pos u (i, x) t) = Pos (fmap f u) (i, f x) (fmap f t)

instance Foldable ZipSeq where
  foldr f z (Pos u (i, x) t) = foldr f (f x $ foldr f z t) u

prevZS :: ZipSeq a -> Maybe (ZipSeq a)
prevZS (Pos (s :|> x) (i, y) u) = Just $ Pos s (i - 1, x) (y :<| u)
prevZS _ = Nothing

nextZS :: ZipSeq a -> Maybe (ZipSeq a)
nextZS (Pos s (i, x) (y :<| u)) = Just $ Pos (s :|> x) (i + 1, y) u
nextZS _ = Nothing

cyclicNextZS :: ZipSeq a -> ZipSeq a
cyclicNextZS (Pos s (i, x) (y :<| u)) = Pos (s :|> x) (i + 1, y) u
cyclicNextZS (Pos (x :<| s) (i, y) Empty) = Pos Empty (0, x) (s :|> y)

moveToZS :: Int -> ZipSeq a -> ZipSeq a
moveToZS i (Pos s (j, x) u)
  | i == j = Pos s (j, x) u
  | i < j =
    let (t, (w :<| ws)) = Data.Sequence.splitAt i s
     in Pos t (i, w) ((ws :|> x) >< u)
  | i > j =
    let (t, (w :<| ws)) = Data.Sequence.splitAt (i - j - 1) u
     in Pos ((s :|> x) >< t) (i, w) ws

valueZS :: ZipSeq a -> a
valueZS (Pos _ (_, x) _) = x

lookupZS :: Int -> ZipSeq a -> Maybe a
lookupZS i (Pos s (j, x) u)
  | i == j = Just x
  | i < j = s !? i
  | i > j = u !? (i - j - 1)

updateZS :: Int -> a -> ZipSeq a -> ZipSeq a
updateZS i z (Pos s (j, x) u)
  | i == j = Pos s (j, z) u
  | i < j = Pos (update i z s) (j, x) u
  | i > j = Pos s (j, x) $ update (i - j - 1) z u

overZS :: (a -> a) -> ZipSeq a -> ZipSeq a
overZS f (Pos s (i, x) u) = Pos s (i, f x) u

indexZS :: ZipSeq a -> Int
indexZS (Pos _ (i, _) _) = i

atEndZS :: ZipSeq a -> Bool
atEndZS (Pos _ _ Empty) = True
atEndZS _ = False

fromSeq :: Seq a -> Maybe (ZipSeq a)
fromSeq (x :<| xs) = Just $ Pos Empty (0, x) xs
fromSeq _ = Nothing

toSeq :: ZipSeq a -> Seq a
toSeq (Pos s (_, x) u) = (s :|> x) >< u
