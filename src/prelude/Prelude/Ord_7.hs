module Prelude(Ord(..)) where

instance (Ord a,Ord b,Ord c,Ord d,Ord e,Ord f,Ord g) =>
	 Ord (a,b,c,d,e,f,g) where
  compare (a,b,c,d,e,f,g)  (a',b',c',d',e',f',g') =
    case compare a a' of
	EQ -> case compare b b' of
	    EQ -> case compare c c' of
		EQ -> case compare d d' of
		    EQ -> case compare e e' of
		        EQ -> case compare f f' of
		            EQ -> compare g g'
		            neq -> neq
		        neq -> neq
		    neq -> neq
		neq -> neq
	    neq -> neq
	neq -> neq

