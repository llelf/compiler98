module Prelude(Bounded(..)) where

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f) =>
	 Bounded (a,b,c,d,e,f) where
  minBound = (minBound, minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)
