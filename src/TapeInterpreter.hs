{-# LANGUAGE FlexibleContexts #-}

module TapeInterpreter
  ( module TapeInterpreter.Core
  , module TapeInterpreter.IntCode
  , module TapeInterpreter.IntOps
  , module TapeInterpreter.Types
  , makeIntProgram
  , makeIntProgramM
  ) where

import Control.Monad.Except
import qualified Data.IntMap.Strict as IM
import Data.Sequence
import TapeInterpreter.Core
import TapeInterpreter.IntCode
import TapeInterpreter.IntOps
import TapeInterpreter.Types

ops :: IM.IntMap (Operation Int)
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

makeIntProgram :: Int -> Seq Int -> [Int] -> Maybe (ProgramState Int)
makeIntProgram t p is = makeProgram t ops p is

makeIntProgramM ::
     MonadError ProgramError m
  => Int
  -> Seq Int
  -> [Int]
  -> m (ProgramState Int)
makeIntProgramM t p is =
  case makeIntProgram t p is of
    Nothing -> throwError EmptyProgram
    Just q -> return q
