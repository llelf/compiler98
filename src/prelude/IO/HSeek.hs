module IO (hSeek) where

import SeekMode
import DHandle
import NHC.FFI

-- #if !defined(TRACING)
#if 1

foreign import ccall hSeekC :: Handle -> Int -> Integer -> IO Int

hSeek                 :: Handle -> SeekMode -> Integer -> IO () 
hSeek h s i = do
    x <- hSeekC h (fromEnum s) i
    if x/=0 then do
        errno <- getErrNo
        throwIOError ("hSeek "++show s++" "++show i) Nothing (Just h) errno
      else
        return ()

#else

foreign import ccall hSeekC :: ForeignObj -> Int -> Integer -> IO Int

hSeek                 :: Handle -> SeekMode -> Integer -> IO () 
hSeek (Handle h) s i = do
    x <- hSeekC h (fromEnum s) i
    if x/=0 then do
        errno <- getErrNo
        throwIOError ("hSeek "++show s++" "++show i) Nothing
                                                 (Just (Handle h)) errno
      else
        return ()

#endif
