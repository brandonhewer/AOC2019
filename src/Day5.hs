{-# LANGUAGE FlexibleContexts #-}

module Day5 where

import Control.Monad.Except (runExcept)
import Data.Functor.Classes (eq1, liftEq)
import qualified Data.IntMap.Strict as IM
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Sequence
import qualified IntCode as IC
import IntOps

ops :: IM.IntMap (IC.Operation Int)
ops =
  IM.fromList
    [ (1, addOperation)
    , (2, multiplyOperation)
    , (3, readOperation)
    , (4, outputOperation)
    , (5, jumpIfTrue)
    , (6, jumpIfFalse)
    , (7, lessThanOperation)
    , (8, equalityOperation)
    ]

makeIntSeq :: [String] -> Seq Int
makeIntSeq = fromList . (map read . splitOn "," =<<)

run :: Seq Int -> [Int] -> Either IC.ProgramError ([Int])
run p =
  (IC.outputs . IC.environment <$>) . (runExcept . IC.runProgramWith 99 ops p)

solve :: Int -> [String] -> String
solve n xs =
  case run (makeIntSeq xs) [n] of
    Left err -> show err
    Right os -> show os
