module IO (hGetChar,cHGetChar) where

import DIO
import DIOError
import DHandle

#if !defined(TRACING)
import PreludeBuiltin(_hGetChar)

hGetChar              :: Handle -> IO Char
hGetChar h    = IO (\world -> input h)
 where
  input h = let c = cHGetChar h
            in if c < 0 then
                 Left (IOErrorEOF h "hGetChar")
               else
                 Right (toEnum c)

cHGetChar :: Handle -> Int
cHGetChar h = _hGetChar h		-- _hGetChar -> special bytecode


#else

hGetChar              :: Handle -> IO Char
hGetChar (Handle h)    = IO (const (input h))
                    --   IO (\world -> input h)  -- const gives nicer traces
 where
  input h = let c = cHGetChar h
            in if c < 0 then
                 Left (IOErrorEOF (Handle h) "hGetChar")
               else
                 Right (toEnum c)

cHGetChar :: ForeignObj -> Int
cHGetChar handle = _prim _tprim_chGetChar handle
_tprim_chGetChar primitive 2 :: Trace -> R ForeignObj -> R Int

#endif
