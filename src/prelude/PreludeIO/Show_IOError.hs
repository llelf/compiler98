module Prelude(Show(..)) where

import CString
import PackedString
import DIOError
import IO
import DErrNo

instance  Show IOError  where
    showsPrec p (IOErrorUser str)      = showString "User-defined IO error:" . showString str
    showsPrec p (IOErrorSystem cstr errno) = 
					 showString "Could not do system(" 
				       . showString (fromCString cstr) . showString "), error code " 
				       . shows errno . showChar '.'
    showsPrec p (IOErrorOpen file mode errno) =
					 showString "Could not open " 
				       . showString (fromCString file) . showString " in mode " 
				       . showString (unpackPS mode) . showString ", error code "
				       . shows errno . showChar '.'
    showsPrec p (IOErrorEOF handle op) = showString "End of file detected in " 
				       . showString op . showString " on "
				       . (case hGetFileName handle of
					    Nothing -> showString "un-named handle"
					    Just fn -> showString fn) . showChar '.'
    showsPrec p (IOErrorHIsEOF handle errno)        = errmsg "hIsEOF" handle errno
    showsPrec p (IOErrorHFileSize handle errno)     = errmsg "hFileSize" handle errno
    showsPrec p (IOErrorHFlush handle errno)        = errmsg "hFlush" handle errno
    showsPrec p (IOErrorHSeek handle errno)         = errmsg "hSeek" handle errno
    showsPrec p (IOErrorHGetPosn handle errno)      = errmsg "hGetPosn" handle errno
    showsPrec p (IOErrorHSetPosn handle errno)      = errmsg "hSetPosn" handle errno
    showsPrec p (IOErrorHGetBuffering handle errno) = errmsg "hGetBuffering" handle errno
    showsPrec p (IOErrorHSetBuffering handle errno) = errmsg "hSetBuffering" handle errno
    showsPrec p (IOErrorC errno) = showString "IO system error (" .
                                   shows (fromEnum errno) . showString "): " .
                                   showString (show errno)

    showsType a = showString "IOError"

errmsg :: String -> Handle -> Int -> ShowS
errmsg str handle errno = 
   showString "IO operation " . showString str 
   . showString " on " 
   . (case hGetFileName handle of
        Nothing -> showString "un-named handle"
        Just fn -> showString fn)
   . showString " failed, error code " . shows errno . showChar '.'
