module Prelude(Eq(..)) where

--import PrimIntegerEq
--import PrimIntegerNe

instance Eq Integer where
  a == b = _prim _tprim_EqInteger a b -- primIntegerEq a b 
  a /= b = _prim _tprim_NEqInteger a b -- primIntegerNe a b 

_tprim_EqInteger primitive 3 :: Trace -> R Integer -> R Integer -> R Bool
_tprim_NEqInteger primitive 3 :: Trace -> R Integer -> R Integer -> R Bool
