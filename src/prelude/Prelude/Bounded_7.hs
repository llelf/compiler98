module Prelude(Bounded(..)) where

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f,
	  Bounded g) =>
	 Bounded (a,b,c,d,e,f,g) where
  minBound = (minBound,minBound,minBound,minBound,minBound,minBound,minBound)
  maxBound = (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound)
