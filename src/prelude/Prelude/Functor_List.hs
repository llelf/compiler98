module Prelude(Functor(..)) where

import Map
import CFunctor

instance Functor [] where
#if !defined(TRACING)
    fmap = map
#else
    fmap f xs = map f xs
#endif
