module Prelude(cOpen,primOpenFile,Handle,IOMode,FilePath) where

import IO
import CString
import DIO

cOpen primitive 2 :: CString -> IOMode -> (Either IOError Handle)

primOpenFile :: FilePath -> IOMode -> IO Handle
primOpenFile fp iomode = 
  IO ( \ world -> cOpen (toCString fp) iomode )
