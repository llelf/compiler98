module Prelude(Enum(..)) where

instance Enum Float where
  toEnum = fromIntegral
  fromEnum = fromInteger . truncate

  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
