module IO (hGetContents) where

import DIO
import DHandle
import HGetChar(cHGetChar)

#if !defined(TRACING)
import PreludeBuiltin (_hGetStr)

{-
nhc currently doesn't implement semi-closed handles.
Hence using hGetContents twice for the same handle will lead
to obscure results.
-}

hGetContents            :: Handle -> IO String
hGetContents h           = IO (\world -> Right (cHGetStr h))
{-
 where
  input h = let c = cHGetChar h
            in if c < 0 then
                 []			-- EOF here
               else
                 toEnum c : input h
-}

cHGetStr :: Handle -> [Char]
cHGetStr h = _hGetStr h		-- _hGetStr becomes a special bytecode

#else

hGetContents            :: Handle -> IO String
hGetContents (Handle h)  = IO (\world -> (Right (input h)))
 where
  input h = let c = cHGetChar h
            in if c < 0 then
                 []			-- EOF here
               else
                 toEnum c : input h

#endif
