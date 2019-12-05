{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module IntCode where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import Data.List (uncons)
import Data.Sequence (Seq, update)
import IntUtil (digits)
import ZipSequence

class Environment m a | m -> a where
  isTerminator :: a -> m Bool
  operation :: IM.Key -> m (Operation a)
  readInput :: m a
  output :: a -> m ()

class Program m a | m -> a where
  next :: m ()
  moveTo :: Int -> m ()
  value :: m a
  valueAt :: Int -> m a
  updateAt :: Int -> a -> m ()
  index :: m Int

data NaryF r a where
  Unary :: (a -> r) -> NaryF r a
  Nary :: (a -> NaryF r a) -> NaryF r a

data Env a =
  Env
    { terminator :: a
    , inputs :: [a]
    , outputs :: [a]
    , operations :: IM.IntMap (Operation a)
    }

data ProgramState a =
  PState
    { program :: ZipSeq a
    , environment :: Env a
    }

data Effect a
  = Store Int a
  | Read Int
  | Output Int
  | MoveTo Int
  | Continue

data ParameterMode
  = Position
  | Immediate
  deriving (Enum)

data ProgramError
  = EndOfProgram
  | OutOfBounds Int
  | InputNotFound
  | NotAnOperation Int
  | EmptyProgram
  | Debug String
  deriving (Show, Eq)

type Operation a = NaryF (Effect a) a

instance (MonadError ProgramError m, MonadState (ProgramState a) m, Eq a) =>
         Environment m a where
  isTerminator n = ((== n) . terminator . environment) <$> get
  operation i = do
    PState _ e <- get
    case IM.lookup i (operations e) of
      Nothing -> throwError (NotAnOperation i)
      Just op -> return op
  readInput = do
    PState p (Env t is os ops) <- get
    case is of
      [] -> throwError InputNotFound
      (i:xs) -> (put $ PState p (Env t xs os ops)) >> return i
  output o = do
    PState p (Env t is os ops) <- get
    put $ PState p (Env t is (o : os) ops)

instance (MonadError ProgramError m, MonadState (ProgramState a) m) =>
         Program m a where
  value = (valueZS . program) <$> get
  index = (indexZS . program) <$> get
  next = do
    PState p e <- get
    case nextZS p of
      Nothing -> throwError EndOfProgram
      Just v -> put (PState v e)
  moveTo i = do
    PState p e <- get
    put $ PState (moveToZS i p) e
  valueAt i = do
    PState p _ <- get
    case lookupZS i p of
      Nothing -> throwError (OutOfBounds i)
      Just v -> return v
  updateAt i z = do
    PState p e <- get
    put $ PState (updateZS i z p) e

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
