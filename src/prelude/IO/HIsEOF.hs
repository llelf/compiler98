module IO (hIsEOF) where

import DHandle

-- #if !defined(TRACING)
#if 0
foreign import ccall hIsEOF :: Handle -> IO Bool

#else
foreign import ccall "hIsEOF" hIsEOFC :: ForeignObj -> IO Bool

hIsEOF :: Handle -> IO Bool
hIsEOF (Handle h) = hIsEOFC h

#endif
