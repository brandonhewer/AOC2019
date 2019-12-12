{-# LANGUAGE FlexibleContexts #-}

module Day9 where

import Control.Monad.Except (runExcept)
import Data.List.Split (splitOn)
import Data.Sequence
import qualified TapeInterpreter as IC

makeIntSeq :: [String] -> Seq Int
makeIntSeq = fromList . (map read . splitOn "," =<<)

solve1 :: Int -> [String] -> String
solve1 n xs =
  case IC.makeIntProgramM 99 (makeIntSeq xs) [n] >>=
       runExcept . IC.runIntProgram of
    Left err -> show err
    Right os -> show $ IC.outputs (IC.environment os)
