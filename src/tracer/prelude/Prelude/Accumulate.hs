module Prelude where

accumulate 	:: Monad m => [m a] -> m [a]
accumulate	= foldr mcons (return [])
		  where mcons p q = p >>= \x -> q >>= \y -> return (x:y)
