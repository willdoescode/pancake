{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Expr where

import Data.Int

class CustomAddition a where
  (.+) :: a -> a -> a

instance CustomAddition Int8 where
  (.+) = (+)

instance CustomAddition Int16 where
  (.+) = (+)

instance CustomAddition Int32 where
  (.+) = (+)

instance CustomAddition Int64 where
  (.+) = (+)

instance CustomAddition Float where
  (.+) = (+)

instance CustomAddition [a] where
  (.+) = (++)

data PExpression a where
  PType :: PType a -> PExpression a
  PFunc :: PFunc a -> PExpression a

data PType a where
  PInt8 :: Int8 -> PType Int8
  PInt16 :: Int16 -> PType Int16
  PInt32 :: Int32 -> PType Int32
  PInt64 :: Int64 -> PType Int64
  PFloat :: Float -> PType Float
  PBoolean :: Bool -> PType Bool
  PChar :: Char -> PType Char
  PList :: [PExpression a] -> PType [a]
  PString :: [Char] -> PType [Char]

data PFunc a where
  PAddition :: forall a. CustomAddition a => PExpression a -> PExpression a -> PFunc a
  PMultiplacation :: forall a. Num a => PExpression a -> PExpression a -> PFunc a
  PEquals :: forall a. Eq a => PExpression a -> PExpression a -> PFunc Bool

evalPType :: PType a -> a
evalPType (PInt8 n) = n
evalPType (PInt16 n) = n
evalPType (PInt32 n) = n
evalPType (PInt64 n) = n
evalPType (PBoolean b) = b
evalPType (PFloat f) = f
evalPType (PChar c) = c
evalPType (PString l) = l
evalPType (PList l) = map eval l

eval :: PExpression a -> a
eval (PFunc (PAddition a b)) = eval a .+ eval b
eval (PFunc (PMultiplacation a b)) = eval a * eval b
eval (PFunc (PEquals a b)) = eval a == eval b
eval (PType a) = evalPType a
