module Prelude where

lines	   	:: String -> [String]
lines ""	= []
lines s		= let (l,s') = break (== '\n') s 
		  in l : case s' of
			      []      -> []
			      (_:s'') -> lines s''
{-
lines s = lines' [] s
    where lines' l ('\n':xs) = reverse l : lines' [] xs
          lines' l (x:xs) = lines' (x:l) xs
	  lines' [] [] = []
	  lines' l  [] = reverse l:[]
-}

