module Prelude where

zip3	:: [a] -> [b] -> [c] -> [(a,b,c)] 
zip3 	= zipWith3 (,,)
