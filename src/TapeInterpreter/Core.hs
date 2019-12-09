{-# LANGUAGE TupleSections #-}

module TapeInterpreter.Core where

import Control.Applicative (liftA2)
import Control.Monad.State.Strict
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IM
import Data.Sequence (Seq, fromList)
import IntUtil (digits)
import TapeInterpreter.Types
import ZipSequence

untilM :: Monad m => m Bool -> m ()
untilM p = do
  continue <- p
  if continue
    then untilM p
    else return ()

getOp ::
     (Monad m, Program m a, Environment m a, Eq a)
  => IM.Key
  -> m ([Int], Operation a)
getOp n =
  case digits n of
    i:[] -> ([], ) <$> operation i
    i:j:ms -> (ms, ) <$> operation (10 * j + i)

isHalted :: (Monad m, Program m a, Environment m a, Eq a) => m Bool
isHalted = value >>= isTerminator

isTerminated :: Eq a => ProgramState a -> Bool
isTerminated (PState p (Env t _ _ _)) = valueZS p == t

getOutputs :: ProgramState a -> [a]
getOutputs (PState _ (Env _ _ os _)) = os

clearOutput :: ProgramState a -> ProgramState a
clearOutput (PState p (Env t is _ ops)) = PState p (Env t is [] ops)

addOutputs :: ProgramState a -> ProgramState a -> ProgramState a
addOutputs p (PState q (Env t xs os op)) =
  PState q (Env t xs (getOutputs p ++ os) op)

mapInputs :: ([a] -> [a]) -> ProgramState a -> ProgramState a
mapInputs f (PState p (Env t xs os op)) = PState p (Env t (f xs) os op)

mapOutToIn ::
     ([a] -> [a] -> [a]) -> ProgramState a -> ProgramState a -> ProgramState a
mapOutToIn = (mapInputs .) . (. getOutputs)

addOutToIn :: ProgramState a -> ProgramState a -> ProgramState a
addOutToIn = mapOutToIn (++)

makeProgram ::
     a -> IM.IntMap (Operation a) -> Seq a -> [a] -> Maybe (ProgramState a)
makeProgram t ops p is = flip PState (Env t is [] ops) <$> fromSeq p

runProgram ::
     Monad m
  => StateT (ProgramState a) m Bool
  -> ProgramState a
  -> m (ProgramState a)
runProgram step = execStateT (untilM step)

runWithInput ::
     (Monad m, Eq a)
  => [a]
  -> StateT (ProgramState a) m Bool
  -> ProgramState a
  -> m (ProgramState a)
runWithInput is step p = runProgram step (mapInputs (is ++) p)

runWithConfig ::
     (Monad m, Eq a)
  => [[a]]
  -> StateT (ProgramState a) m Bool
  -> ProgramState a
  -> m (ProgramState a)
runWithConfig [] step p = return p
runWithConfig (x:xs) step p = do
  q <- runWithInput x step p
  runWithConfig xs step $ (addOutputs q . addOutToIn q) p

pipeOutput :: ZipSeq (ProgramState a) -> ZipSeq (ProgramState a)
pipeOutput ps =
  let os = getOutputs (valueZS ps)
      qs = overZS clearOutput ps
      rs = cyclicNextZS qs
   in overZS (mapInputs (++ reverse os)) rs

runSeqLoop ::
     (Monad m, Eq a)
  => StateT (ProgramState a) m Bool
  -> ZipSeq (ProgramState a)
  -> m (ZipSeq (ProgramState a))
runSeqLoop step ps = do
  r <- runProgram step (valueZS ps)
  let rs = overZS (const r) ps
  if isTerminated r && atEndZS rs
    then return rs
    else runSeqLoop step (pipeOutput rs)

runInLoop ::
     (Monad m, Eq a)
  => StateT (ProgramState a) m Bool
  -> [ProgramState a]
  -> m [ProgramState a]
runInLoop step ps =
  case fromSeq (fromList ps) of
    Nothing -> return []
    Just zs -> toList <$> (toSeq <$> runSeqLoop step zs)

runLoopWithConfig ::
     (Monad m, Eq a)
  => [[a]]
  -> StateT (ProgramState a) m Bool
  -> ProgramState a
  -> m [ProgramState a]
runLoopWithConfig is step p =
  traverse (\i -> runWithInput i step p) is >>= runInLoop step

scanM :: Monad m => (b -> a -> m b) -> m b -> [a] -> m [b]
scanM f z [] = pure <$> z
scanM f z (x:xs) = liftA2 (:) z (scanM f (z >>= flip f x) xs)
