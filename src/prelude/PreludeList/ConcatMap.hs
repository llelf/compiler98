module Prelude where

import Concat
import Map

concatMap  	:: (a->[b]) -> [a] -> [b]
#if !defined(TRACING)
concatMap f	= concat . map f
#else
concatMap f x	= (concat . map f) x
#endif
