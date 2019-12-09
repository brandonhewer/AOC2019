{-# LANGUAGE TupleSections #-}

module Day7 where

import Control.Monad.Except
import Control.Monad.Loops (iterateUntilM)
import Data.List (permutations)
import Data.List.Split (splitOn)
import Data.Sequence (Seq, fromList)
import qualified TapeInterpreter as IC
import ZipSequence

makeIntSeq :: [String] -> Seq Int
makeIntSeq = fromList . (map read . splitOn "," =<<)

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x:xs) = f x : xs

configRun :: Seq Int -> [Int] -> Either IC.ProgramError (IC.ProgramState Int)
configRun p cs =
  IC.makeIntProgramM 99 p [0] >>= runExcept . IC.runIntWithConfig (map pure cs)

loopRun :: Seq Int -> [Int] -> Either IC.ProgramError [IC.ProgramState Int]
loopRun p cs =
  IC.makeIntProgramM 99 p [] >>=
  runExcept . IC.loopIntWithConfig (mapHead (++ [0]) $ map pure cs)

catEither :: [Either a b] -> [b]
catEither [] = []
catEither ((Left _):xs) = catEither xs
catEither ((Right x):xs) = x : catEither xs

solve1 :: [String] -> String
solve1 xs =
  let intseq = makeIntSeq xs
      endstates = map (configRun intseq) (permutations [0 .. 4])
      results = map (IC.outputs . IC.environment <$>) endstates
   in show (maximum $ map head $ catEither results)

solve2 :: [String] -> String
solve2 xs =
  let intseq = makeIntSeq xs
      endstates = map (loopRun intseq) (permutations [5 .. 9])
      results = map (IC.outputs . IC.environment . last <$>) endstates
   in show (maximum $ map head $ catEither results)
