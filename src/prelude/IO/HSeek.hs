module IO (hSeek) where

import SeekMode
import DHandle
import DIOError
import HGetFileName
import FFI

#if !defined(TRACING)

foreign import hSeekC :: Handle -> Int -> Integer -> IO Int

hSeek                 :: Handle -> SeekMode -> Integer -> IO () 
hSeek h s i = do
    x <- hSeekC h (fromEnum s) i
    if x/=0 then do
        errno <- getErrNo
        mkIOError ("hSeek"++show s++" "++show i) (hGetFileName h) (Just h) errno
      else
        return ()

#else

foreign import hSeekC :: ForeignObj -> Int -> Integer -> IO Int

hSeek                 :: Handle -> SeekMode -> Integer -> IO () 
hSeek (Handle h) s i = do
    x <- hSeekC h (fromEnum s) i
    if x/=0 then do
        errno <- getErrNo
        mkIOError ("hSeek"++show s++" "++show i) (hGetFileName h)
                                                 (Just (Handle h)) errno
      else
        return ()

#endif
