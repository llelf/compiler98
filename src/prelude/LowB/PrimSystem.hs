module System where

import CString
import DIOError(IOError(IOErrorSystem))
import DIO
import System

cSystem primitive 1 :: CString -> Either Int ExitCode

primSystem str =
  IO ( \ world ->
            let cstr = toCString str
	    in case cSystem cstr of
	         Left errno -> Left (IOErrorSystem cstr errno)
	         Right code -> Right code)

{-
int cSystem (char *cmd) {
    err = system(cmd);
    if (err==-1) return errno;
}

import DErrNo

foreign import "cSystem" cSystem :: CString -> IO (Either .. ..)
-}
