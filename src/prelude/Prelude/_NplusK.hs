module Prelude where

import PrimIntegerLe
import PrimIntegerSub

-- used when compiling (n+k)-patterns
_leInteger a b   = primIntegerLe a b 
_subInteger a b  = primIntegerSub a b 

#if 0
-- used when compiling (n+k)-patterns
_leInteger a b   = _prim _tprim_IntegerLe a b 
_subInteger a b  = _prim _tprim_IntegerMinus a b 

_tprim_IntegerLe  primitive 3 :: Trace -> R Integer -> R Integer -> R Bool
_tprim_IntegerMinus primitive 3 :: Trace -> R Integer -> R Integer -> R Integer
#endif
