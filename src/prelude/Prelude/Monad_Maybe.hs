module Prelude(Monad(..)) where

import CMonad

instance Monad Maybe where
    (Just x) >>= k	= k x
    Nothing  >>= k	= Nothing
#if !defined(TRACING)
    return		= Just
#else
    return x = Just x  -- no undersaturated data constructor
#endif
    fail s              = Nothing

