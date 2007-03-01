module Prelude where

foldl	   	 :: (a -> b -> a) -> a -> [b] -> a
foldl f z [] 	 = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl'	   	 :: (a -> b -> a) -> a -> [b] -> a
foldl' f z []     = z
foldl' f z (x:xs) = let y = f z x in y `seq` foldl' f y xs
