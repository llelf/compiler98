module Prelude(Enum(..)) where

instance Enum Int where
  toEnum = id
  fromEnum = id

  enumFrom =  numericEnumFrom
  enumFromThen = numericEnumFromThen
