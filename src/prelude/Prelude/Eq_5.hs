module Prelude(Eq(..)) where

instance (Eq a, Eq b, Eq c, Eq d, Eq e) =>
	 Eq (a,b,c,d,e) where
  (a,b,c,d,e) == (a',b',c',d',e') =
	 a==a' && b==b' && c==c' && d==d' && e==e'
