module Prelude(Ord(..)) where

instance (Ord a,Ord b,Ord c,Ord d) => Ord (a,b,c,d) where
  compare (a,b,c,d)  (a',b',c',d') =
    case compare a a' of
    	LT -> LT
	EQ -> case compare b b' of
		LT -> LT
		EQ -> case compare c c' of
		        LT -> LT
			EQ -> compare d d'
			GT -> GT
		GT -> GT
	GT -> GT

