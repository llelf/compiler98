module IO (hPutChar) where

import DIO
import DHandle

#if !defined(TRACING)
hPutChar              :: Handle -> Char -> IO ()
hPutChar h c           = IO (\world -> cHPutChar h c)

cHPutChar h c = _hPutChar h c	-- _hPutChar -> special bytecode

#else
hPutChar              :: Handle -> Char -> IO ()
hPutChar (Handle h) c  = IO (const (cHPutChar h c))
	-- const gives much nicer output-tracing than an explicit lambda

cHPutChar handle ch = _prim _tprim_chPutChar handle ch
_tprim_chPutChar primitive 3 :: Trace -> R ForeignObj -> R Char
                                                      -> R (Either IOError ())
#endif

