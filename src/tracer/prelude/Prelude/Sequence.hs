module Prelude where

sequence 	:: Monad m => [m a] -> m ()
sequence	= foldr (>>) (return ())
