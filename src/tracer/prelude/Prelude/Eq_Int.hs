module Prelude(Eq(..)) where

instance Eq Int where
  a == b = _prim _tprim_IntEq a b
  a /= b = _prim _tprim_IntNEq a b

_tprim_IntEq primitive 3 :: Trace -> R Int -> R Int -> R Bool
_tprim_IntNEq primitive 3 :: Trace -> R Int -> R Int -> R Bool
