module IO (hGetContents) where

import DIO
import DHandle
import HGetChar(cHGetChar)

#if !defined(TRACING)

hGetContents            :: Handle -> IO String
hGetContents h           = IO (\world -> Right (input h))
 where
  input h = let c = cHGetChar h
            in if c < 0 then
                 []			-- EOF here
               else
                 toEnum c : input h

#else

hGetContents            :: Handle -> IO String
hGetContents (Handle h)  = IO (const (Right (input h)))
 where
  input h = let c = cHGetChar h
            in if c < 0 then
                 []			-- EOF here
               else
                 toEnum c : input h

#endif
