{-# LANGUAGE FlexibleContexts #-}

module Day2 where

import Control.Monad.Except (runExcept)
import Data.Functor.Classes (eq1, liftEq)
import qualified Data.IntMap.Strict as IM
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Sequence
import qualified IntCode as IC

ops :: IM.IntMap (IC.Operation Int)
ops =
  IM.fromList
    [ (1, IC.Nary $ \x -> IC.Unary (\y -> x + y))
    , (2, IC.Nary $ \x -> IC.Unary (\y -> x * y))
    ]

makeIntSeq :: [String] -> Seq Int
makeIntSeq = fromList . (map read . splitOn "," =<<)

run :: Int -> Int -> Seq Int -> Either IC.ProgramError (Seq Int)
run m n xs = runExcept $ IC.runProgramWith 99 ops [(1, m), (2, n)] xs

isHead :: Eq a => a -> Seq a -> Bool
isHead x y = eq1 (Just x) (y !? 0)

solve1 :: [String] -> String
solve1 xs =
  case run 12 2 (makeIntSeq xs) of
    Left err -> show err
    Right rs -> show $ index rs 0

solve2 :: [String] -> String
solve2 xs =
  let program = makeIntSeq xs
      results = [run x y program | x <- [1 .. 100], y <- [1 .. 100]]
   in case find (liftEq isHead $ Right 19690720) results of
        Nothing -> "Not found"
        Just (Left err) -> show err
        Just (Right x) -> show (100 * index x 1 + index x 2)
