module Prelude where

#if !defined(TRACING)
import PrimIntegerEq

-- used when compiling case-expressions if pattern is known to be Integer
_eqInteger a b  = primIntegerEq a b 

#else
-- used when compiling case-expressions if pattern is known to be Integer
_eqInteger a b  = _prim _tprim_EqInteger a b 

_tprim_EqInteger primitive 3 :: Trace -> R Integer -> R Integer -> R Bool

#endif
