module Prelude(Eq(..)) where

#if !defined(TRACING)
instance Eq Float where
  a == b = a == b    --- MAGIC
  a /= b = a /= b    --- MAGIC
#else

import PrimsFloat

instance Eq Float where
  a == b = primFloatEq a b
  a /= b = primFloatNe a b


#if 0
  a == b = _prim _tprim_EqFloat a b  -- a == b    --- MAGIC
  a /= b = _prim _tprim_NEqFloat a b  -- a /= b    --- MAGIC

_tprim_EqFloat primitive 3 :: Trace -> R Float -> R Float -> R Bool
_tprim_NEqFloat primitive 3 :: Trace -> R Float -> R Float -> R Bool
#endif
#endif
