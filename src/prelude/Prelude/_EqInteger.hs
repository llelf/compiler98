module Prelude where

import PrimIntegerEq

-- used when compiling case-expressions if pattern is known to be Integer
foreign import "primIntegerEqC" _eqInteger :: Integer -> Integer -> Bool

