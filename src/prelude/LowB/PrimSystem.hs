module System where

import CString
import DIOError(IOError(IOErrorSystem))
import DIO
import System

cSystem primitive 1 :: PackedString -> Either Int ExitCode

primSystem str =
  IO ( \ world ->
            let cstr = toCString str
	    in case cSystem cstr of
	         Left errno -> Left (IOErrorSystem cstr errno)
	         Right code -> Right code)


{-
-- a possible future implementation in terms of common FFI?
import FFI
import DErrNo

{-# CCODE #include <stdlib.h> #-}
{-# CCODE extern int system (const char *cmd); #-}
foreign import ccall "system" cSystem :: CString -> IO Int
primSystem cmd = do
    v <- cSystem cmd
    if v==(-1) || v==127
      then do e <- getErrNo
              ioError (IOErrorC ("system "++show str) Nothing e)
      else return v
-}
