module Prelude(Enum(..)) where

instance Enum () where
    fromEnum () = 0
    toEnum   0  = ()
    toEnum   n  = error ("Enum.Unit.toEnum on " ++ show n)

    enumFrom n        = [()]
    enumFromThen n n' = [()]
