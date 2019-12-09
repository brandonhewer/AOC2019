{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module TapeInterpreter.IntCode where

import Control.Applicative (liftA2)
import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import Data.List (uncons)
import Data.Sequence (Seq, update)
import TapeInterpreter.Core
import TapeInterpreter.Types
import ZipSequence

getValue ::
     (Monad m, Program m Int, Environment m Int) => Maybe ParameterMode -> m Int
getValue (Just Immediate) = value
getValue _ = value >>= valueAt

runEffect ::
     (Monad m, Program m Int, Environment m Int)
  => Maybe ParameterMode
  -> Effect Int
  -> m ()
runEffect _ (Store i v) = (updateAt i v) >> next
runEffect _ (Read i) = (readInput >>= updateAt i) >> next
runEffect _ (Output i) = (valueAt i >>= output) >> next
runEffect (Just Immediate) (MoveTo i) = moveTo i
runEffect _ (MoveTo i) = valueAt i >>= moveTo
runEffect _ Continue = next

runOp ::
     (Monad m, Program m Int, Environment m Int)
  => [ParameterMode]
  -> Operation Int
  -> m ()
runOp ms (Unary f) = value >>= runEffect (fst <$> uncons ms) . f
runOp ms (Nary f) = do
  v <- getValue (fst <$> uncons ms)
  next >> runOp (drop 1 ms) (f v)

isAwaitingInput :: (Monad m, Program m Int, Environment m Int) => m Bool
isAwaitingInput = do
  (_, op) <- value >>= getOp
  case op of
    Unary f -> do
      v <- ((1 +) <$> index) >>= valueAt
      case f v of
        Read _ -> noInputs
        _ -> return False
    _ -> return False

stepInt :: (Monad m, Program m Int, Environment m Int) => m Bool
stepInt = do
  halted <- isHalted
  if halted
    then return False
    else do
      wait <- isAwaitingInput
      if wait
        then return False
        else do
          (ms, op) <- value >>= getOp
          next >> runOp (map toEnum ms) op
          return True

runIntProgram ::
     MonadError ProgramError m => ProgramState Int -> m (ProgramState Int)
runIntProgram p = runProgram stepInt p

runIntWithConfig ::
     MonadError ProgramError m
  => [[Int]]
  -> ProgramState Int
  -> m (ProgramState Int)
runIntWithConfig cs = runWithConfig cs stepInt

loopIntWithConfig ::
     MonadError ProgramError m
  => [[Int]]
  -> ProgramState Int
  -> m [ProgramState Int]
loopIntWithConfig cs = runLoopWithConfig cs stepInt

modifyProgram :: [(Int, a)] -> Seq a -> Seq a
modifyProgram = flip (foldr (uncurry update))
