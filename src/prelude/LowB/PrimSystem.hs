module System where

import CString
import DIOError
import DIO
import System

cSystem primitive 1 :: CString -> Either Int ExitCode

primSystem str =
  IO ( \ world ->
            let cstr = toCString str
	    in case cSystem cstr of
	         Left errno -> Left (IOErrorSystem cstr errno)
	         Right code -> Right code)

