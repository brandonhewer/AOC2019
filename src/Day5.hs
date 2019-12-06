{-# LANGUAGE FlexibleContexts #-}

module Day5 where

import Control.Monad.Except (runExcept)
import Data.Functor.Classes (eq1, liftEq)
import qualified Data.IntMap.Strict as IM
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Sequence
import qualified TapeInterpreter as IC

ops :: IM.IntMap (IC.Operation Int)
ops =
  IM.fromList
    [ (1, IC.addOperation)
    , (2, IC.multiplyOperation)
    , (3, IC.readOperation)
    , (4, IC.outputOperation)
    , (5, IC.jumpIfTrue)
    , (6, IC.jumpIfFalse)
    , (7, IC.lessThanOperation)
    , (8, IC.equalityOperation)
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
