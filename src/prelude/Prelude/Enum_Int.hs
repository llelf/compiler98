module Prelude(Enum(..)) where

instance Enum Int where
  toEnum = id
  fromEnum = id

#if !defined(TRACING)
  enumFrom =  numericEnumFrom
  enumFromThen = numericEnumFromThen
#else
  enumFrom n = n : enumFrom (n+1)
  enumFromThen m n = m : enumFromThen n (2*n-m)
#endif
