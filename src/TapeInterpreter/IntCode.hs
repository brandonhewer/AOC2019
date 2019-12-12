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

valueAtOr ::
     (MonadError ProgramError m, Program m Int, Environment m Int)
  => Int
  -> Int
  -> m Int
valueAtOr m n = valueAt n `catchError` handle
  where
    handle (OutOfBounds i)
      | i < 0 = throwError (OutOfBounds i)
      | otherwise = return m
    handle err = throwError err

getValue ::
     (MonadError ProgramError m, Program m Int, Environment m Int)
  => Maybe ParameterMode
  -> Int
  -> m Int
getValue (Just Immediate) x = return x
getValue (Just Relative) x = relativePos x >>= valueAtOr 0
getValue _ x = valueAtOr 0 x

runEffect ::
     (MonadError ProgramError m, Program m Int, Environment m Int)
  => Maybe ParameterMode
  -> Effect Int
  -> m ()
runEffect (Just Relative) (Store i v) =
  relativePos i >>= flip updateAt v >> next
runEffect _ (Store i v) = updateAt i v >> next
runEffect (Just Relative) (Read i) =
  relativePos i >>= (readInput >>=) . updateAt >> next
runEffect _ (Read i) = (readInput >>= updateAt i) >> next
runEffect p (Output i) = (getValue p i >>= output) >> next
runEffect p (IncrementBase i) =
  (getValue p i >>= adjustRelativeBase . (+)) >> next
runEffect p (MoveTo i) = getValue p i >>= moveTo
runEffect _ Continue = next

runOp ::
     (MonadError ProgramError m, Program m Int, Environment m Int)
  => [ParameterMode]
  -> Operation Int
  -> m ()
runOp ms (Unary f) = value >>= runEffect (fst <$> uncons ms) . f
runOp ms (Nary f) = do
  v <- value >>= getValue (fst <$> uncons ms)
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

stepInt ::
     (MonadError ProgramError m, Program m Int, Environment m Int) => m Bool
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
