module IO (hFileSize) where

import DHandle
import DIOError
import HGetFileName

#if !defined(TRACING)
foreign import primHFileSizeC :: Handle -> Either Int Integer

hFileSize :: Handle -> IO Integer
hFileSize h = _mkIOwf1 (IOErrorC "hFileSize" (hGetFileName h) . toEnum)
                           primHFileSizeC h

#else
foreign import primHFileSizeC :: ForeignObj -> Either Int Integer

hFileSize :: Handle -> IO Integer
hFileSize (Handle h) = _mkIOwf1 (IOErrorC "hFileSize" (hGetFileName h) . toEnum)
                           primHFileSizeC h
#endif
