module Prelude(Ord(..)) where

instance (Ord a,Ord b,Ord c,Ord d,Ord e) =>
	 Ord (a,b,c,d,e) where
  compare (a,b,c,d,e)  (a',b',c',d',e') =
    case compare a a' of
	EQ -> case compare b b' of
	    EQ -> case compare c c' of
		EQ -> case compare d d' of
		    EQ -> compare e e'
		    neq -> neq
		neq -> neq
	    neq -> neq
	neq -> neq

