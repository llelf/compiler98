module IO where

import IO
import DIOError

ioeGetHandle :: IOError -> Maybe Handle
ioeGetHandle (IOErrorUser             str)       = Nothing
ioeGetHandle (IOErrorSystem           str errno) = Nothing
ioeGetHandle (IOErrorOpen       file mode errno) = Nothing
ioeGetHandle (IOErrorEOF           handle op)    = Just handle
ioeGetHandle (IOErrorHIsEOF        handle errno) = Just handle
ioeGetHandle (IOErrorHFileSize     handle errno) = Just handle
ioeGetHandle (IOErrorHFlush        handle errno) = Just handle
ioeGetHandle (IOErrorHSeek         handle errno) = Just handle
ioeGetHandle (IOErrorHGetPosn      handle errno) = Just handle
ioeGetHandle (IOErrorHSetPosn      handle errno) = Just handle
ioeGetHandle (IOErrorHGetBuffering handle errno) = Just handle
ioeGetHandle (IOErrorHSetBuffering handle errno) = Just handle
