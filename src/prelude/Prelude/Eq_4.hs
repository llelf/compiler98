module Prelude(Eq(..)) where

instance (Eq a,Eq b,Eq c,Eq d) => Eq (a,b,c,d) where
  (a,b,c,d) == (a',b',c',d') = a == a' && b == b' && c == c' && d == d'
