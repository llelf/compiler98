module Prelude(Eq(..)) where

instance Eq Ordering where
  a  == b = fromEnum a == fromEnum b
{-
  EQ == EQ = True
  LT == LT = True
  GT == GT = True
  _  == _  = False
-}
