{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module IntCode where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import Data.Sequence
import ZipSequence

class IntEnvironment m where
  isTerminator :: Int -> m Bool
  operation :: Int -> m (Operation Int)

class IntProgram m where
  next :: m ()
  value :: m Int
  valueAt :: Int -> m Int
  updateAt :: Int -> Int -> m ()
  index :: m Int

data Operation a where
  Unary :: (a -> a) -> Operation a
  Nary :: (a -> Operation a) -> Operation a

data OpEnv a =
  Env
    { terminator :: a
    , operations :: IM.IntMap (Operation a)
    }

data ProgramState a =
  PState
    { program :: ZipSeq a
    , environment :: OpEnv a
    }

data ProgramError
  = EndOfProgram
  | OutOfBounds Int
  | NotAnOperation Int
  | EmptyProgram
  | Debug String
  deriving (Show, Eq)

instance (MonadError ProgramError m, MonadState (ProgramState Int) m) =>
         IntEnvironment m where
  isTerminator n = ((== n) . terminator . environment) <$> get
  operation i = do
    PState _ e <- get
    case IM.lookup i (operations e) of
      Nothing -> throwError (NotAnOperation i)
      Just op -> return op

instance (MonadError ProgramError m, MonadState (ProgramState Int) m) =>
         IntProgram m where
  value = (valueZS . program) <$> get
  index = (indexZS . program) <$> get
  next = do
    PState p e <- get
    case nextZS p of
      Nothing -> throwError EndOfProgram
      Just v -> put (PState v e)
  valueAt i = do
    PState p _ <- get
    case lookupZS i p of
      Nothing -> throwError (OutOfBounds i)
      Just v -> return v
  updateAt i z = do
    PState p e <- get
    put $ PState (updateZS i z p) e

runOp :: (Monad m, IntProgram m, IntEnvironment m) => Operation Int -> m Int
runOp (Unary f) = f <$> (value >>= valueAt)
runOp (Nary f) = do
  v <- value >>= valueAt
  next >> runOp (f v)

step :: (Monad m, IntProgram m, IntEnvironment m) => m Bool
step = do
  op <- value >>= operation
  next
  res <- runOp op
  next >> value >>= flip updateAt res
  next >> value >>= (not <$>) . isTerminator

runProgram' :: (Monad m, IntProgram m, IntEnvironment m) => m ()
runProgram' = do
  continue <- step
  if continue
    then runProgram'
    else return ()

runProgram ::
     MonadError ProgramError m
  => Int
  -> IM.IntMap (Operation Int)
  -> Seq Int
  -> m (Seq Int)
runProgram t ops p =
  case fromSeq p of
    Nothing -> throwError EmptyProgram
    Just zs ->
      (toSeq . program) <$> execStateT runProgram' (PState zs $ Env t ops)

runProgramWith ::
     MonadError ProgramError m
  => Int
  -> IM.IntMap (Operation Int)
  -> [(Int, Int)]
  -> Seq Int
  -> m (Seq Int)
runProgramWith t ops is p = runProgram t ops (foldr (uncurry update) p is)
