{-# LANGUAGE FlexibleContexts #-}

module Day5 where

import Control.Monad.Except (runExcept)
import Data.List.Split (splitOn)
import Data.Sequence
import qualified TapeInterpreter as IC

makeIntSeq :: [String] -> Seq Int
makeIntSeq = fromList . (map read . splitOn "," =<<)

solve :: Int -> [String] -> String
solve n xs =
  case IC.makeIntProgramM 99 (makeIntSeq xs) [n] >>=
       runExcept . IC.runIntProgram of
    Left err -> show err
    Right os -> show $ IC.outputs (IC.environment os)
