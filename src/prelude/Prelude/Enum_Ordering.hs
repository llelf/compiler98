module Prelude(Enum(..)) where

#if !defined(TRACING)
import _EnumFromTo
import _EnumFromThen
#endif

instance Enum Ordering where
    fromEnum LT = 0
    fromEnum EQ = 1
    fromEnum GT = 2

#if !defined(TRACING)
    toEnum   0 = LT
    toEnum   1 = EQ
    toEnum   2 = GT

    enumFrom n        = _enumFromTo n 2
    enumFromThen n n' = _enumFromThen n n' 2

#else
    toEnum n = if n == 0 then LT else if n == 1 then EQ else GT
#endif
