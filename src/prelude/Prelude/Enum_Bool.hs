module Prelude(Enum(..)) where

#if !defined(TRACING)
import _EnumFromTo
import _EnumFromThen
#endif


instance Enum Bool where
    fromEnum   False = 0
    fromEnum   True  = 1

#if defined(TRACING)
    toEnum n = if n == 0 then False else if n == 1 then True else
	       error ("Enum.Bool.toEnum on " ++ show n)
#else
    toEnum   0 = False
    toEnum   1 = True
    toEnum   n = error ("(Prelude.toEnum "++show n++" :: Bool) is wrong")

    enumFrom n        = _enumFromTo  n 1
    enumFromThen n n' = _enumFromThen n n' 1
#endif
