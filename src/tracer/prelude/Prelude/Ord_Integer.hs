module Prelude(Ord(..)) where

--import PrimIntegerLt
--import PrimIntegerLe
--import PrimIntegerGe
--import PrimIntegerGt

instance Ord Integer where
  a <  b = _prim _tprim_IntegerLt a b -- primIntegerLt a b 
  a <= b = _prim _tprim_IntegerLe a b -- primIntegerLe a b
  a >= b = _prim _tprim_IntegerGe a b -- primIntegerGe a b 
  a >  b = _prim _tprim_IntegerGt a b -- primIntegerGt a b 

_tprim_IntegerLt primitive 3 :: Trace -> R Integer -> R Integer -> R Bool
_tprim_IntegerLe primitive 3 :: Trace -> R Integer -> R Integer -> R Bool
_tprim_IntegerGe primitive 3 :: Trace -> R Integer -> R Integer -> R Bool
_tprim_IntegerGt primitive 3 :: Trace -> R Integer -> R Integer -> R Bool
