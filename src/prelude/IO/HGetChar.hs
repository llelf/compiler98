module IO (hGetChar,cHGetChar) where

import DIO
import DIOError
import DHandle

{-
The world ensures correct sequentialisation of IO-actions,
especially avoids sharing of input actions.
However, the world is not passed everywhere and the following
code relies on nhc not perform optimisations that could
change the evaluation order.
E.g., don't use `const' instead of the lambda abstraction of world.
-}

#if !defined(TRACING)
import PreludeBuiltin(_hGetChar)

hGetChar              :: Handle -> IO Char
hGetChar h    = IO (\world -> input h)
 where
  input h = let c = cHGetChar h
            in if c < 0 then
                 Left (EOFError "hGetChar" h)
               else
                 Right (toEnum c)

cHGetChar :: Handle -> Int
cHGetChar h = _hGetChar h		-- _hGetChar -> special bytecode


#else

hGetChar              :: Handle -> IO Char
hGetChar (Handle h)    = IO (input h)
 where
  input h world = 
    let c = cHGetChar h
    in if c < 0 
         then Left (EOFError "hGetChar" (Handle h))
         else Right (toEnum c)

foreign import ccall "_chGetChar" cHGetChar :: ForeignObj -> Int

{-
cHGetChar :: ForeignObj -> Int
cHGetChar handle = _prim _tprim_chGetChar handle
_tprim_chGetChar primitive 2 :: Trace -> R ForeignObj -> R Int
-}

#endif
