module Prelude(Eq(..)) where

#if !defined(TRACING)
import PrimIntegerEq
import PrimIntegerNe
#endif

instance Eq Integer where
#if !defined(TRACING)
  a == b = primIntegerEq a b 
  a /= b = primIntegerNe a b 
#else
  a == b = _prim _tprim_EqInteger a b -- primIntegerEq a b 
  a /= b = _prim _tprim_NEqInteger a b -- primIntegerNe a b 

_tprim_EqInteger primitive 3 :: Trace -> R Integer -> R Integer -> R Bool
_tprim_NEqInteger primitive 3 :: Trace -> R Integer -> R Integer -> R Bool
#endif
