module IO (hPutChar) where

import NHC.Internal (IO(..))

#if !defined(TRACING)
import PreludeBuiltin (Handle)

hPutChar              :: Handle -> Char -> IO ()
hPutChar h c           = IO (\world -> cHPutChar h c)

cHPutChar :: Handle -> Char -> Either IOError ()
cHPutChar h c = _hPutChar h c	-- _hPutChar -> special bytecode

#else
import DHandle

hPutChar              :: Handle -> Char -> IO ()
{-
hPutChar (Handle h) c  = cHPutChar h c
foreign import ccall "chPutChar" cHPutChar :: ForeignObj -> Char -> IO ()
-}

hPutChar (Handle h) c  = IO (cHPutChar h c)

cHPutChar handle ch world = _prim _tprim_chPutChar handle ch
_tprim_chPutChar primitive 3 :: Trace -> R ForeignObj -> R Char
                                                      -> R (Either IOError ())

#endif

