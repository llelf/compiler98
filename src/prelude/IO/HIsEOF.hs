module IO (hIsEOF) where

import DHandle

#if 0
-- if !defined(TRACING)
foreign import hIsEOF :: Handle -> IO Bool

#else
foreign import "hIsEOF" hIsEOFC :: ForeignObj -> IO Bool

hIsEOF :: Handle -> IO Bool
hIsEOF (Handle h) = hIsEOFC h

#endif
