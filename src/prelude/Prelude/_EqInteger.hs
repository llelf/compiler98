module Prelude where

import PrimIntegerEq

-- used when compiling case-expressions if pattern is known to be Integer

_eqInteger a b  = primIntegerEq a b 
