{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module TapeInterpreter.IntCode where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import Data.List (uncons)
import Data.Sequence (Seq, update)
import IntUtil (digits)
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

getOp ::
     (Monad m, Program m a, Environment m a, Eq a, Enum b)
  => IM.Key
  -> m ([b], Operation a)
getOp n =
  case digits n of
    i:[] -> ([], ) <$> operation i
    i:j:ms -> (map toEnum ms, ) <$> operation (10 * j + i)

step :: (Monad m, Program m Int, Environment m Int) => m Bool
step = do
  (ms, op) <- value >>= getOp
  next >> runOp ms op
  value >>= (not <$>) . isTerminator

runProgram' :: (Monad m, Program m Int, Environment m Int) => m ()
runProgram' = do
  continue <- step
  if continue
    then runProgram'
    else return ()

runProgramWith ::
     MonadError ProgramError m
  => Int
  -> IM.IntMap (Operation Int)
  -> Seq Int
  -> [Int]
  -> m (ProgramState Int)
runProgramWith t ops p is =
  case fromSeq p of
    Nothing -> throwError EmptyProgram
    Just zs -> execStateT runProgram' (PState zs $ Env t is [] ops)

runProgram ::
     MonadError ProgramError m
  => Int
  -> IM.IntMap (Operation Int)
  -> Seq Int
  -> m (ProgramState Int)
runProgram t ops p = runProgramWith t ops p []

runOnce ::
     MonadError ProgramError m
  => Int
  -> IM.IntMap (Operation Int)
  -> Seq Int
  -> m (Seq Int)
runOnce t ops p = (toSeq . program) <$> runProgram t ops p

modifyProgram :: [(Int, a)] -> Seq a -> Seq a
modifyProgram = flip (foldr (uncurry update))
