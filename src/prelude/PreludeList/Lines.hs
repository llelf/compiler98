module Prelude where

lines	   	:: String -> [String]
lines ""	= []
lines s		= let (l,s') = break (== '\n') s 
		  in l : case s' of
			      []      -> []
			      (_:s'') -> lines s''
