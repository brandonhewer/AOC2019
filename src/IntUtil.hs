module IntUtil where

digits :: Integral a => a -> [a]
digits 0 = [0]
digits n = go n
  where
    go 0 = []
    go n = n `mod` 10 : go (n `div` 10)
