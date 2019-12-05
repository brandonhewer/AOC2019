module IntOps where

import IntCode (Effect(..), NaryF(..), Operation)

binaryOp :: (Int -> Int -> Int) -> Operation Int
binaryOp f = Nary $ \x -> Nary $ \y -> Unary $ \i -> Store i (f x y)

predicateOp :: (Int -> Int -> Bool) -> Operation Int
predicateOp p = binaryOp ((fromEnum .) . p)

addOperation :: Operation Int
addOperation = binaryOp (+)

multiplyOperation :: Operation Int
multiplyOperation = binaryOp (*)

lessThanOperation :: Operation Int
lessThanOperation = predicateOp (<)

equalityOperation :: Operation Int
equalityOperation = predicateOp (==)

readOperation :: Operation Int
readOperation = Unary $ \i -> Read i

outputOperation :: Operation Int
outputOperation = Unary $ \i -> Output i

jumpIf :: (Int -> Bool) -> Operation Int
jumpIf p =
  Nary $ \x ->
    Unary $ \i ->
      if p x
        then MoveTo i
        else Continue

jumpIfTrue :: Operation Int
jumpIfTrue = jumpIf (/= 0)

jumpIfFalse :: Operation Int
jumpIfFalse = jumpIf (== 0)
