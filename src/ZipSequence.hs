module ZipSequence where

import Data.Sequence

data ZipSeq a =
  Pos (Seq a) (Int, a) (Seq a)
  deriving (Show)

nextZS :: ZipSeq a -> Maybe (ZipSeq a)
nextZS (Pos s (i, x) (y :<| u)) = Just $ Pos (s :|> x) (i + 1, y) u
nextZS _ = Nothing

valueZS :: ZipSeq a -> a
valueZS (Pos _ (_, x) _) = x

lookupZS :: Int -> ZipSeq a -> Maybe a
lookupZS i (Pos s (j, x) u)
  | i == j = Just x
  | i < j = s !? i
  | i > j = u !? (i - j)

updateZS :: Int -> a -> ZipSeq a -> ZipSeq a
updateZS i z (Pos s (j, x) u)
  | i == j = Pos s (j, z) u
  | i < j = Pos (update i z s) (j, x) u
  | i > j = Pos s (j, x) $ update (i - j) z u

indexZS :: ZipSeq a -> Int
indexZS (Pos _ (i, _) _) = i

fromSeq :: Seq a -> Maybe (ZipSeq a)
fromSeq (x :<| xs) = Just $ Pos Empty (0, x) xs
fromSeq _ = Nothing

toSeq :: ZipSeq a -> Seq a
toSeq (Pos s (_, x) u) = (s :|> x) >< u
