{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module TapeInterpreter.Types where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import ZipSequence

class Environment m a | m -> a where
  isTerminator :: a -> m Bool
  operation :: IM.Key -> m (Operation a)
  noInputs :: m Bool
  readInput :: m a
  output :: a -> m ()
  relativePos :: Int -> m Int
  adjustRelativeBase :: (Int -> Int) -> m ()

class Program m a | m -> a where
  next :: m ()
  prev :: m ()
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
    , relativeBase :: Int
    }

data ProgramState a =
  PState
    { program :: ZipSeq a
    , environment :: Env a
    , externalMemory :: IM.IntMap a
    }

data Effect a
  = Store Int a
  | Read Int
  | Output Int
  | MoveTo Int
  | IncrementBase Int
  | Continue

data ParameterMode
  = Position
  | Immediate
  | Relative
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
    PState _ e _ <- get
    case IM.lookup i (operations e) of
      Nothing -> throwError (NotAnOperation i)
      Just op -> return op
  noInputs = do
    PState _ (Env _ is _ _ _) _ <- get
    return (null is)
  readInput = do
    PState p (Env t is os ops r) m <- get
    case is of
      [] -> throwError InputNotFound
      (i:xs) -> (put $ PState p (Env t xs os ops r) m) >> return i
  output o = do
    PState p (Env t is os ops r) m <- get
    put $ PState p (Env t is (o : os) ops r) m
  relativePos n = do
    PState _ (Env _ _ _ _ r) _ <- get
    return (r + n)
  adjustRelativeBase f = do
    PState p (Env t is os ops r) m <- get
    put $ PState p (Env t is os ops (f r)) m

instance (MonadError ProgramError m, MonadState (ProgramState a) m) =>
         Program m a where
  value = (valueZS . program) <$> get
  index = (indexZS . program) <$> get
  next = do
    PState p e m <- get
    case nextZS p of
      Nothing -> throwError EndOfProgram
      Just v -> put (PState v e m)
  prev = do
    PState p e m <- get
    case prevZS p of
      Nothing -> index >>= throwError . OutOfBounds
      Just v -> put (PState v e m)
  moveTo i = do
    PState p e m <- get
    put $ PState (moveToZS i p) e m
  valueAt i = do
    PState p _ m <- get
    case lookupZS i p of
      Nothing ->
        case IM.lookup i m of
          Nothing -> throwError (OutOfBounds i)
          Just v -> return v
      Just v -> return v
  updateAt i z = do
    PState p e m <- get
    if i < lengthZS p
      then put $ PState (updateZS i z p) e m
      else put $ PState p e (IM.insert i z m)
