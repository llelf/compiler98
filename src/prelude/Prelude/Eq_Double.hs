module Prelude(Eq(..)) where

#if defined(TRACING)
import PrimsDouble
#endif

instance Eq Double where
#if !defined(TRACING)
  a == b = a == b    --- MAGIC
  a /= b = a /= b    --- MAGIC
#else
  a == b = primDoubleEq a b
  a /= b = primDoubleNe a b

#if 0
  a == b = _prim _tprim_EqDouble a b -- a == b    --- MAGIC
  a /= b = _prim _tprim_NEqDouble a b -- a /= b    --- MAGIC

_tprim_EqDouble primitive 3 :: Trace -> R Double -> R Double -> R Bool
_tprim_NEqDouble primitive 3 :: Trace -> R Double -> R Double -> R Bool
#endif
#endif
