module Prelude(Ord(..)) where

instance (Ord a,Ord b,Ord c,Ord d,Ord e,Ord f) =>
	 Ord (a,b,c,d,e,f) where
  compare (a,b,c,d,e,f)  (a',b',c',d',e',f') =
    case compare a a' of
	EQ -> case compare b b' of
	    EQ -> case compare c c' of
		EQ -> case compare d d' of
		    EQ -> case compare e e' of
		        EQ -> compare f f'
		        neq -> neq
		    neq -> neq
		neq -> neq
	    neq -> neq
	neq -> neq

