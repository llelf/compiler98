module Prelude where

repeat		:: a -> [a]
repeat x 	= xs where xs = x:xs
--repeat x 	= x : repeat x
