module Prelude(Bounded(..)) where

instance (Bounded a, Bounded b, Bounded c, Bounded d) =>
	 Bounded (a,b,c,d) where
  minBound = (minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound)
