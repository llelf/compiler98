module Prelude where

import Sequence

mapM_           :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f as      =  sequence (map f as)
