module Prelude where
 
_foldr :: (a -> b -> b) -> [a] -> b -> b
_foldr f [] d = d
_foldr f ((:) x xs) d = f x (_foldr f xs d)
