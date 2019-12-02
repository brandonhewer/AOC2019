{-# LANGUAGE FlexibleContexts #-}

module Day2 where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing)
import Data.Sequence

codeOp :: MonadError String m => Int -> m (Int -> Int -> Int)
codeOp 1 = return (+)
codeOp 2 = return (*)
codeOp n = throwError ("No operation with code " ++ show n)

innerIndex :: Seq Int -> Int -> Int
innerIndex vs i = index vs (index vs i)

runStep :: (MonadState (Seq Int) m, MonadError String m) => Int -> m (Maybe Int)
runStep n = do
  vs <- get
  case vs !? (n + 3) of
    Nothing -> throwError ("Index out of bounds: " ++ show n)
    Just pos ->
      case index vs n of
        99 -> return Nothing
        c -> do
          op <- codeOp (index vs n)
          let rs = op (innerIndex vs (n + 1)) (innerIndex vs (n + 2))
          put $ update pos rs vs
          return $ Just (n + 4)

iterateUntilM :: Monad m => (a -> m (Maybe a)) -> a -> m a
iterateUntilM f v = f v >>= go
  where
    go Nothing = return v
    go (Just w) = iterateUntilM f w

runProgram :: (MonadState (Seq Int) m, MonadError String m) => m ()
runProgram = iterateUntilM runStep 0 >> return ()

runWithInput :: MonadError String m => Int -> Int -> Seq Int -> m (Seq Int)
runWithInput n v = execStateT runProgram . update 2 v . update 1 n

makeIntSeq :: [String] -> Seq Int
makeIntSeq = fromList . (map read . splitOn "," =<<)

solve1 :: [String] -> String
solve1 xs =
  let vs = runWithInput 12 2 $ makeIntSeq xs
   in case runExcept vs of
        Left err -> err
        Right rs -> show $ index rs 0

solve2 :: [String] -> String
solve2 xs =
  let program = makeIntSeq xs
      results = [runWithInput x y program | x <- [1 .. 100], y <- [1 .. 100]]
   in case find isExpRes results of
        Nothing -> "Not found"
        Just v ->
          case runExcept v of
            Left err -> err
            Right x -> show (100 * index x 1 + index x 2)
  where
    isExpRes r =
      case runExcept r of
        Left err -> False
        Right rs -> 19690720 == index rs 0
