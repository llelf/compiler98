module Prelude where

class (Num a, Ord a) => Real a where
  toRational		:: a -> Rational
