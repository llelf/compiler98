module Prelude(Eq(..)) where

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
	 Eq (a,b,c,d,e,f,g) where
  (a,b,c,d,e,f,g) == (a',b',c',d',e',f',g') =
	 a==a' && b==b' && c==c' && d==d' && e==e' && f==f' && g==g'
