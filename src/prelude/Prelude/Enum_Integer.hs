module Prelude(Enum(..)) where

instance Enum Integer where
  succ x     = x+1
  pred x     = x-1
  toEnum x   = toInteger x
  fromEnum x = fromInteger x

  enumFrom x = numericEnumFrom x
  enumFromThen x y = numericEnumFromThen x y
