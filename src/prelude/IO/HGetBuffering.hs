module IO (hGetBuffering) where

import DHandle
import BufferMode

-- #if !defined(TRACING)
#if 1
foreign import ccall "hGetBufferingC" hGetBuffering :: Handle -> IO BufferMode

#else
foreign import ccall hGetBufferingC :: ForeignObj -> IO BufferMode

hGetBuffering :: Handle -> IO BufferMode	-- BufferMode not done yet.
hGetBuffering (Handle h) = hGetBufferingC h

#endif
