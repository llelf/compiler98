module Prelude where

import Accumulate

mapM            :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as       =  accumulate (map f as)

