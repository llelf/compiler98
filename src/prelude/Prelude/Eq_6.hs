module Prelude(Eq(..)) where

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
	 Eq (a,b,c,d,e,f) where
  (a,b,c,d,e,f) == (a',b',c',d',e',f') =
	 a==a' && b==b' && c==c' && d==d' && e==e' && f==f'
