module IO (hSeek) where

import SeekMode
import DHandle
import DIOError
import HGetFileName

#if !defined(TRACING)

foreign import hSeekC :: Handle -> Int -> Integer -> Either Int ()

hSeek                 :: Handle -> SeekMode -> Integer -> IO () 
hSeek h s i = _mkIOwf3 (IOErrorC ("hSeek "++show s++" "++show i)
                                 (hGetFileName h) . toEnum)
                  hSeekC h (fromEnum s) i

#else

foreign import hSeekC :: ForeignObj -> Int -> Integer -> Either Int ()

hSeek                 :: Handle -> SeekMode -> Integer -> IO () 
hSeek (Handle h) s i = _mkIOwf3 (IOErrorC ("hSeek "++show s++" "++show i)
                                          (hGetFileName h) . toEnum)
                           hSeekC h (fromEnum s) i

#endif
