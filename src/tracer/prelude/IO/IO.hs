module IO (
     Handle, 
--     HandlePosn,
--     IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),
--     BufferMode(NoBuffering,LineBuffering,BlockBuffering),
--     SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),
     stdin, stdout, stderr, 
--     openFilehFileSize, hIsEOF, isEOF,
     hClose, 
--     hSetBuffering, hGetBuffering, hFlush, hGetPosn, hSetPosn, hSeek, 
--     hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable, hReady, 
     hGetChar, 
--     hLookAhead, 
     hGetContents, hPutChar, hPutStr
--     hPrint,
--     isAlreadyExistsError, isAlreadyInUseError, isFullError, isEOFError,
--     isIllegalOperation, isPermissionError, isUserError, 
--     ioeGetHandle, ioeGetFileName,hGetFileName,
--     SocketType(..), openSocket
     ) where

--import Ix
--import LowIO

import PreludeBuiltin(Handle,stdin,stdout,stderr)

--import DHandlePosn
--import Eq_HandlePosn
--import IOMode
--import BufferMode
--import SeekMode

--import OpenFile
import HClose
--import HFileSize
--import HIsEOF
--import IsEOF
--import HSetBuffering
--import HGetBuffering
--import HFlush
--import HGetPosn
--import HSetPosn
--import HSeek
import HGetContents
import HPutChar
import HPutStr
--import HPrint
import HGetChar
--import IsUserError
--import IsEOFError
--import IoeGetHandle
--import IoeGetFileName
--import HGetFileName
--import OpenSocket
--import DSocket

--import NotDoneYet

