module Prelude(Enum(..)) where

instance Enum Double where
  toEnum = fromIntegral
  fromEnum = fromInteger . truncate

  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
