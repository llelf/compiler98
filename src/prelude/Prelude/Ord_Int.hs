module Prelude(Ord(..)) where

#if !defined(TRACING)

instance Ord Int where
  a <  b = a <  b -- MAGIC
  a <= b = a <= b -- MAGIC
  a >= b = a >= b -- MAGIC
  a >  b = a >  b -- MAGIC

#else

instance Ord Int where
  a <  b = _prim _tprim_IntLT a b
  a <= b = _prim _tprim_IntLE a b
  a >= b = _prim _tprim_IntGE a b
  a >  b = _prim _tprim_IntGT a b

_tprim_IntLT primitive 3 :: Trace -> R Int -> R Int -> R Bool
_tprim_IntLE primitive 3 :: Trace -> R Int -> R Int -> R Bool
_tprim_IntGE primitive 3 :: Trace -> R Int -> R Int -> R Bool
_tprim_IntGT primitive 3 :: Trace -> R Int -> R Int -> R Bool

#endif
