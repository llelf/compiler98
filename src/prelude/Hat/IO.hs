module IO (
     Handle
    ,HandlePosn
    ,IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode)
    ,BufferMode(NoBuffering,LineBuffering,BlockBuffering)
    ,SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd)
    ,stdin, stdout, stderr, hClose
    ,openFile, hFileSize, hIsEOF, isEOF
    ,hSetBuffering, hGetBuffering
    ,hFlush, hGetPosn, hSetPosn, hSeek
    ,hWaitForInput, hReady, hGetLine, hLookAhead
    ,hGetChar
    ,hGetContents, hPutChar, hPutStr, hPutStrLn, hPrint
    ,hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable
    ,isAlreadyExistsError, isDoesNotExistError
    ,isAlreadyInUseError, isFullError, isEOFError
    ,isIllegalOperation, isPermissionError, isUserError
    ,ioeGetErrorString, ioeGetHandle, ioeGetFileName
    ,try, bracket, bracket_
  ) where

import Ix
import IOPrimitives (Handle,HandlePosn,openFile,hClose,hFileSize,hFlush
                    ,hGetBuffering,hGetChar,hGetContents,hGetPosn,hIsEOF
                    ,hPutChar,hSeek ,hSetBuffering,hSetPosn
                    ,ioeGetErrorString,ioeGetFileName,ioeGethandle
                    ,isEOFError,isAlreadyExistsError,isDoesNotExistError
                    ,isAlreadyInUseError,isFullError,isIllegalOperationError
                    ,isPermissionError,isUserError
                    ,hWaitForInput,hReady,hLookAhead,hIsOpen,hIsClosed
                    ,hIsReadable,hIsWritable,hIsSeekable
                    ,stdin,stdout,stderr
                    )

data BufferMode  =  NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
                     deriving (Eq, Ord, Read, Show)

data IOMode      =  ReadMode | WriteMode | AppendMode | ReadWriteMode
                     deriving (Eq, Ord, Ix, Bounded, Enum, Read, Show)

data SeekMode    =  AbsoluteSeek | RelativeSeek | SeekFromEnd
                     deriving (Eq, Ord, Ix, Bounded, Enum, Read, Show)

bracket               :: IO a -> (a->IO b) -> (a->IO c) -> IO c
bracket before after m = do
    x <- before
    rs <- try (m x)
    after x
    case rs of
      Right r -> return r
      Left e  -> ioError e

bracket_              :: IO a -> (a->IO b) -> IO c -> IO c
bracket_ before after m = do
    x <- before
    rs <- try m
    after x
    case rs of
      Right r -> return r
      Left e  -> ioError e

try                   :: IO a -> IO (Either IOError a)
try f                  = catch (do r <- f
                                   return (Right r))
                               (return . Left)


hGetLine              :: Handle -> IO String
hGetLine h             = do c  <- hGetChar h
                            case c of
                              '\n' -> return []
                               _   -> do cs <- hGetLine h
                                         return (c:cs)


hPrint                :: Show a => Handle -> a -> IO ()
hPrint h a             = hPutStr h (show a)

hPutStr               :: Handle -> String -> IO ()
hPutStr h []           = return ()
hPutStr h (x:xs)       = hPutChar h x >> hPutStr h xs

hPutStrLn             :: Handle -> String -> IO ()
hPutStrLn h s          =  do hPutStr h s
                             hPutChar h '\n'

isEOF                 :: IO Bool
isEOF                  = hIsEOF stdin

