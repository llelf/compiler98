module Prelude(Eq(..)) where

instance Eq Float where
#if !defined(TRACING)
  a == b = a == b    --- MAGIC
  a /= b = a /= b    --- MAGIC
#else
  a == b = _prim _tprim_EqFloat a b  -- a == b    --- MAGIC
  a /= b = _prim _tprim_NEqFloat a b  -- a /= b    --- MAGIC

_tprim_EqFloat primitive 3 :: Trace -> R Float -> R Float -> R Bool
_tprim_NEqFloat primitive 3 :: Trace -> R Float -> R Float -> R Bool
#endif
