module IO (hFlush) where

import DHandle
import DIOError
import HGetFileName

#if !defined(TRACING)
foreign import hFlushC :: Handle -> Either Int ()

hFlush :: Handle -> IO ()
hFlush h = _mkIOwf1 (IOErrorC "hFlush" (hGetFileName h) . toEnum)
                        hFlushC h

#else
foreign import hFlushC :: ForeignObj -> Either Int ()

hFlush :: Handle -> IO ()
hFlush (Handle h) = _mkIOwf1 (IOErrorC "hFlush" (hGetFileName h) . toEnum)
                        hFlushC h

#endif
